# `coalton/threads`

Threads, synchronization primitives, and a work-stealing runtime for
task and data parallelism.

> [!WARNING]
> `coalton/threads` is experimental, and its interface is subject to
> change. It currently runs on SBCL only.

This library is not part of the standard library loaded by `coalton`.
Load it with

```lisp
(ql:quickload "coalton/threads")
```

or add `"coalton/threads"` to the `:depends-on` list of your system. It
currently runs on SBCL only; loading it on another Lisp signals an
error. See [Porting](#porting) for adding support for other Lisps.

For an introduction to parallel programming with this library, see the
tutorial [Parallel
Programming](https://coalton-lang.github.io/manual/topics/parallel-programming/)
in the Coalton manual
([source](../docs/manual/site/topics/parallel-programming.md)).

| Package | Contents |
|---------|----------|
| `coalton/threads/parallel` | Fork-join (`join`), scoped tasks (`scope`, `spawn`), futures (`async`, `await`), and parallel loops, reductions, maps and sorting |
| `coalton/threads/thread` | Operating-system threads: `spawn`, `join` |
| `coalton/threads/mutex` | `Mutex`, `with-lock` |
| `coalton/threads/condition-variable` | `ConditionVariable` |
| `coalton/threads/semaphore` | `Semaphore` |
| `coalton/threads/atomic` | `Atomic`, a cell with atomic operations |
| `coalton/threads/channel` | `Channel`, an unbounded queue between threads |

## Parallel computation

The functions in `coalton/threads/parallel` run tasks on a pool of
worker threads, one per processor available to the process by default.
The pool starts the first time it is needed.

```lisp
(defpackage #:example
  (:use #:coalton #:coalton-prelude)
  (:local-nicknames
   (#:math #:coalton/math)
   (#:par #:coalton/threads/parallel)))

(in-package #:example)

(coalton-toplevel
  (declare sequential-fib (UFix -> UFix))
  (define (sequential-fib n)
    (if (< n 2)
        n
        (+ (sequential-fib (- n 1)) (sequential-fib (- n 2)))))

  ;; Divide and conquer: JOIN evaluates both functions, potentially in
  ;; parallel, and returns both values.
  (declare fib (UFix -> UFix))
  (define (fib n)
    (if (< n 25)
        (sequential-fib n)
        (progn
          (let (values a b) = (par:join (fn () (fib (- n 1)))
                                        (fn () (fib (- n 2)))))
          (+ a b))))

  ;; Collections: map, reduce and sort in parallel.
  (declare sum-of-squares (Vector Integer -> Integer))
  (define (sum-of-squares v)
    (par:map-reduce (fn (x) (* x x)) + 0 v))

  ;; Loops with cheap iterations: process whole chunks of a range, so
  ;; that each chunk runs as a tight loop with an unboxed accumulator.
  (declare sum-of-sines (UFix -> F64))
  (define (sum-of-sines n)
    (par:map-reduce-chunks
     0 n
     (fn (lo hi)
       (for ((declare i UFix) (declare acc F64)
             (i lo (1+ i)) (acc 0d0 (+ acc (math:sin (fromInt (into i))))))
         :returns acc
         :repeat (- hi lo)
         Unit))
     + 0d0)))
```

The main operations are:

- `join` calls two functions, potentially in parallel, and returns both
  values. It is cheap enough for recursive divide-and-conquer
  algorithms: a join whose second function is not taken by another
  worker costs about 25 ns.
- `scope` and `spawn` run any number of tasks for their effects. `scope`
  returns once every task spawned in it has finished.
- `async` starts a computation and returns a `Future`, and `await`
  returns its value.
- `for-range!`, `map-reduce-range` and `fold-map-range` loop over
  ranges of integers; `for-chunks!` and `map-reduce-chunks` do the same
  a chunk at a time.
- `for-each!`, `map-reduce` and `fold-map` work on the instances of the
  class `ParallelFoldable`: `Seq`, and every `RandomAccess` collection,
  such as `Vector` and `LispArray`. `map-into!` maps one `RandomAccess`
  collection into another. `map`, the method of the class
  `ParallelMap`, maps over a `Vector`, a `List` or a `Seq`, and `sort!`
  and `sort-by!` sort a `Vector` with a stable parallel merge sort.

### Errors

An exception thrown by a task, like any other error that it signals, is
signaled again by the operation waiting for it (`join`, `await`,
`scope`, or the loop or reduction that ran it), so it can be caught
with `catch` or `try` in the usual way:

```lisp
(catch (par:map-reduce parse-or-throw + 0 inputs)
  ((ParseError message) (report message)))
```

When both functions of a `join` fail, the error of the first one is
signaled. If the first function of a `join` exits early, the second is
cancelled if it has not started, and waited for otherwise: no task of a
`join` or `scope` keeps running after the `join` or `scope` returns. So
the cleanup forms of a `protect` around a parallel operation run after
its tasks have finished.

A task may run on another worker thread, where it does not see the
`handle` branches and resumptions established around the operation. A
`handle` around the operation then sees the task's error only when the
operation signals it again, after the task has unwound, so it cannot
resume the task with `resume-to`. Put a `handle` that resumes a task in
the task itself.

If a task is unwound before it finishes, for example because its
worker thread is interrupted, the operation waiting for it signals
`TaskAborted`. Likewise, `thread:join` signals `ThreadAborted` for a
thread that ended without returning. Misuses that the library detects,
such as spawning a task in a scope that has finished, signal a `Panic`.

### Configuration

- `(par:worker-count)` returns the number of workers.
- `(par:set-worker-count! n)` uses `n` workers from now on. The
  environment variable `COALTON_NUM_THREADS` sets the initial number.
- `(par:shutdown!)` stops the workers; they start again when needed.
  The workers are stopped automatically before `save-lisp-and-die`,
  which requires that no other threads be running.
- Setting `coalton/threads/runtime:*debug-tasks*` to true in the REPL
  makes an error in a task enter the debugger in the worker thread that
  ran it, before it is transferred to the waiting thread.

Worker threads write to the output streams that were current when the
pool started.

## Threads and synchronization

The other packages provide conventional concurrency tools. Threads are
typed by the value they compute, and `thread:join` signals any error
that ended the thread:

```lisp
;; With the local nicknames cell (coalton/cell), mutex
;; (coalton/threads/mutex) and thread (coalton/threads/thread):
(coalton-toplevel
  (define (count-in-parallel)
    (let ((lock (mutex:new))
          (counter (cell:new 0))
          (threads (map (fn (_)
                          (thread:spawn
                           (fn ()
                             (for ()
                               :repeat 1000
                               (mutex:with-lock lock
                                 (fn () (cell:increment! counter) (values))))
                             Unit)))
                        (range 1 4))))
      (map thread:join threads)
      (cell:read counter))))
```

`Atomic` offers lock-free updates (`update!`, `compare-and-swap!`,
`increment!`, `push!`, `pop!`), and `Channel` passes values between
threads. Timeouts, as in `channel:recv-timeout!`, are in seconds, as for
`coalton/system:sleep`.

## Guidelines and caveats

- **Granularity.** Parallelism pays when tasks do at least a few
  microseconds of work. Stop splitting recursive problems below some
  size, as in `fib` above, and prefer the chunked loops when
  iterations are cheap.
- **Futures.** Use `join` and `scope` for divide-and-conquer
  algorithms, and futures for independent computations. A task that
  awaits a future running on another worker can only run tasks of its
  own computation in the meantime, so its worker may sit idle.
- **Allocation.** SBCL's garbage collector stops all threads, so code
  that allocates heavily scales less well than code that does not.
  Unboxed arrays (`LispArray F64`) and chunked loops with unboxed
  accumulators help.
- **Data races.** Nothing prevents tasks from mutating the same data
  concurrently. Tasks may write to distinct elements of a vector or
  array, but structural changes, such as `vector:push!` or hash table
  updates, must not happen concurrently with other accesses. The
  elements of a `LispArray Bit` are packed into machine words, so tasks
  must not write elements in the same block of 64 (indices `64k` to
  `64k + 63`) at the same time; `map-into!` respects this. Share
  mutable state through `Atomic`, `Mutex`, or `Channel`.
- **Loop variables.** A `for` loop updates its variables in place, so a
  task created in the body of a `for` that refers to a loop variable may
  see a later value of it. Bind the value with `let` first.
- **Blocking.** A task that blocks, on a mutex, a channel, or the end of
  a thread, keeps its worker from running other tasks. A task must not
  wait for another task except by `join`, `await`, or the end of a
  `scope`; otherwise the program can deadlock.
- **Locks.** Do not hold a mutex while calling a parallel operation. A
  worker that waits inside `join`, `await` or `scope` runs other tasks
  in the meantime, and one of them could try to acquire the same mutex.
- **Dynamic environment.** A task may run on the thread that started
  the operation, or on any worker. Only in the former case does it see
  the dynamic variable bindings, handlers, and resumptions established
  around the operation, so it must not depend on them. Errors are
  transferred as described in [Errors](#errors), but a task cannot
  reliably `resume-to` a resumption established outside it, nor be
  resumed by a `handle` outside it. Parallel operations called from a
  thread outside the pool, such as the REPL, run entirely on worker
  threads while the calling thread waits.
- **The pool.** `set-worker-count!` and `shutdown!` must not be called
  from a task, nor while other threads are running parallel operations.
- **Interrupts.** As SBCL recommends, interrupt worker threads only for
  interactive debugging, and do not use `sb-ext:with-timeout` inside
  tasks: unwinding a worker in the middle of a scheduling operation can
  leave the pool inconsistent. (Unwinding a worker that is running the
  code of a task, or that sleeps for lack of work, is safe.)
- **Unwinding a waiting thread.** A thread waiting for a `join`, a
  `scope` or a parallel loop, including a thread outside the pool, may
  be unwound, for example by an interrupt or a deadline. It then waits
  for the operation's tasks to finish before unwinding further, so that
  no task outlives the operation; tasks that no worker has started yet
  are cancelled where possible. Interrupting the thread again during
  that wait abandons the remaining tasks, which keep running. Awaiting
  a future is different: a thread unwound while it awaits a future
  stops waiting, and the future keeps running.

## Implementation

The runtime, in the package `coalton/threads/runtime`, is written in
Common Lisp. It follows the design of Rayon, the Rust library. Like the
Coalton packages, it depends on the Lisp implementation only through a
small backend (see [Porting](#porting)).

- Each worker owns a Chase-Lev work-stealing deque, using the memory
  orderings of Lê et al. for weak memory models. Workers push and pop
  their own tasks at one end; idle workers steal from the other end of
  a randomly chosen victim's deque. Work submitted by threads outside
  the pool goes through lock-free queues instead.
- `join` pushes its second function onto the worker's deque, calls the
  first, and then pops the second back to call it directly, unless a
  thief took it in the meantime. In that case the worker runs other
  tasks until the stolen one completes, instead of blocking.
- Running other tasks while waiting is only safe if none of them can
  wait, in turn, for the computation buried beneath them on the
  worker's stack. The tasks of `join` and `scope` are only waited for by
  the computation that created them, but futures can be awaited by
  anyone. So each running future forms a domain, to which the `join`
  and `scope` tasks it creates belong, and a waiting worker only runs
  tasks of its own domain. Work outside of any future, including work
  submitted from outside the pool, belongs to a root domain. Futures
  themselves run only at the top level of a worker, or in a thread that
  awaits them before they have started.
- These rules never strand a waiting worker behind work that it may not
  run. A worker waiting for a scope can claim the scope's tasks
  wherever they are, even beneath other work in another worker's deque.
  A task spawned into a scope from another domain goes to the scope's
  inbox, which the scope's owner empties while it waits, rather than to
  the spawning worker's deque, where it could bury work that this
  worker will wait for. So the pool never needs more threads than it
  started with, and programs without futures are scheduled exactly as
  in Rayon.
- A worker runs each task it takes from a deque or from a queue in the
  dynamic environment at the base of its thread, with an `abort`
  restart for that task alone, so that conditions and restarts of
  unrelated computations do not interfere with each other.
- Futures are claimed atomically by whichever thread runs them. A
  worker that awaits a future that has not started runs it itself; one
  that awaits a future in progress runs tasks of its own domain in the
  meantime.
- Loops split their range adaptively: into one or two pieces per
  worker at first, and further only when pieces are stolen, so that
  work is divided finely where idle workers can absorb it and coarsely
  otherwise. A positive `:grain` instead splits the range into fixed
  pieces, which makes reductions of floating-point numbers reproducible.
- Operations on collections are loops over the indices of their
  elements. A `Seq` is a tree, so a chunk of its indices is folded with
  `seq:fold-range`, which descends the tree once to the chunk's first
  element. `map` on a `Seq` uses `seq:map-with`, which builds a tree of
  the same shape and maps each leaf in the chunk that contains its first
  element. Neither function depends on `coalton/threads`: the standard
  library stays independent of it.
- Idle workers search for work for a while, then sleep. Counters of
  searching and sleeping workers let a producer of work decide cheaply
  whether to wake a sleeper: it does so only if no worker is already
  searching. A searcher that finds work wakes up to two sleepers, so
  that the pool comes out of sleep quickly when work keeps appearing.

### Performance

The system `coalton/threads/benchmarks` compares parallel operations
with sequential code:

```lisp
(asdf:load-system "coalton/threads/benchmarks")
(coalton/threads/benchmarks:run-benchmarks)
```

On a 16-core, 32-thread AMD Ryzen AI Max+ PRO 395 with SBCL 2.6.9, using
all 32 hardware threads:

| Benchmark | Speedup over sequential code |
|-----------|-----------------------------:|
| `fib 40`, `join` with a cutoff at 25 | 17x |
| 12-queens, `map-reduce-range` over the first rows | 20x |
| 400×400 `F64` matrix product, `for-range!` over rows | 13–15x |
| Sum of `sin` over 2·10⁷ integers, `map-reduce-chunks` | 18x |
| Sum of `sin` over 2·10⁷ integers, `map-reduce-range` | 6–9x |
| Sort 2·10⁶ integers, `sort!` (vs. `vector:sort!`) | 16x |
| Map over 2·10⁶ integers, `map` (memory bound) | 4–20x |
| Collatz steps of a `Seq` of 2·10⁶ integers, `map-reduce` | 20–22x |
| Map over a `Seq` of 2·10⁶ integers, `map` (memory bound) | 6–7x |
| `fib 30`, `join` at every call | 0.3–0.9x |

The last line shows the cost of parallelism that is too fine-grained:
each call does a few nanoseconds of work, and allocates two closures
and a task.

## Porting

Everything in `coalton/threads` that depends on the Lisp
implementation is in a backend, so that supporting another Lisp
mostly means writing one file:

| File | Loaded by the system | Contents |
|------|----------------------|----------|
| `threads/backend/package.lisp` | every backend | The package `coalton/threads/backend`, which exports the operations that a backend provides, with what each must do |
| `threads/backend/sbcl.lisp` | `coalton/threads/sbcl` | The backend for SBCL |
| `threads/backend/unsupported.lisp` | `coalton/threads/unsupported` | An error, signaled on other Lisps |

The rest, the runtime in `threads/runtime/` and the Coalton packages,
is portable. `coalton/threads` depends first on the backend system for
the Lisp it is loaded in, or else on `coalton/threads/unsupported`, so
that an unsupported Lisp fails before its other dependencies are
loaded.

To port `coalton/threads` to another Lisp, for example Clozure CL:

1. Write `threads/backend/ccl.lisp`, defining every operation exported
   from `coalton/threads/backend` as `package.lisp` describes. Most of
   them map directly onto the threads, locks and atomic operations of
   the implementation.
2. In `coalton.asd`, define a system `coalton/threads/ccl` that loads
   `package.lisp` and `ccl.lisp`, and add `(:feature :ccl
   "coalton/threads/ccl")` to the dependencies of `coalton/threads`.
   Then add `:ccl` to the other feature expressions that list the Lisps
   that `coalton/threads` supports: in the dependency of
   `coalton/threads` on `coalton/threads/unsupported`, in the
   dependencies of `coalton/tests` and `coalton/doc` on
   `coalton/threads`, and on the `threads` module of `coalton/tests`.
3. The tests in `tests/threads/` use a few helpers that depend on the
   implementation, which `support-sbcl.lisp` defines for SBCL. Define
   them in `support-ccl.lisp`, and add it to the `threads` module of
   `coalton/tests`.

## References

- R. D. Blumofe and C. E. Leiserson, "Scheduling Multithreaded
  Computations by Work Stealing", JACM 46(5), 1999.
- D. Chase and Y. Lev, "Dynamic Circular Work-Stealing Deque", SPAA
  2005.
- N. M. Lê, A. Pop, A. Cohen and F. Zappa Nardelli, "Correct and
  Efficient Work-Stealing for Weak Memory Models", PPoPP 2013.
- D. Lea, "A Java Fork/Join Framework", Java Grande 2000.
- Rayon, a data-parallelism library for Rust,
  <https://github.com/rayon-rs/rayon>.
