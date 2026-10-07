---
title: "Parallel Programming"
description: "A tutorial on task and data parallelism with the coalton/threads library."
hideMeta: true
weight: 65
---

<div style="border: 1px solid rgba(217, 119, 6, 0.55); border-left: 6px solid #d97706; border-radius: 4px; background: rgba(245, 158, 11, 0.12); padding: 0.8em 1em; margin: 0 0 1.5em 0;">
<strong>Warning:</strong> <code>coalton/threads</code> is experimental, and its interface is subject to change. It currently runs on SBCL only.
</div>

The `coalton/threads` library runs Coalton code on several processors at
once. Its main package, `coalton/threads/parallel`, provides parallel
loops, reductions, maps and sorting, as well as general tools for running
tasks in parallel: `join`, scopes and futures. All of them run on a pool
of worker threads that balance the work among themselves by *work
stealing*, in the manner of [Rayon](https://github.com/rayon-rs/rayon),
the Rust library. The library also provides operating-system threads,
mutexes, condition variables, semaphores, atomic cells and channels.

This tutorial introduces these tools through a running example, the
Collatz problem. Start with a positive integer. If it is even, halve it;
otherwise, triple it and add 1. Repeat. The [Collatz
conjecture](https://en.wikipedia.org/wiki/Collatz_conjecture) states that
every starting number eventually reaches 1. How many steps that takes
varies erratically: 32 takes 5 steps, but 27 takes 111. That makes
computations over many starting numbers a good test of how well work is
divided among processors.

{{<toc>}}

## Setup

`coalton/threads` is not part of the standard library loaded by
`coalton`. Load it with

```lisp
(ql:quickload "coalton/threads")
```

or add `"coalton/threads"` to the `:depends-on` list of your system. The
library currently runs on SBCL only; loading it on another Lisp signals an
error.

Parallel programming is about speed, so compile your code in release
mode, by setting the environment variable `COALTON_ENV` to `release`
before loading Coalton (see [Configuring
Coalton](/manual/topics/configuring-coalton/)).

The code of this tutorial lives in a package with local nicknames for the
packages it uses:

```lisp
(defpackage #:collatz
  (:use #:coalton #:coalton-prelude)
  (:local-nicknames
   (#:arr #:coalton/lisparray)
   (#:atomic #:coalton/threads/atomic)
   (#:channel #:coalton/threads/channel)
   (#:iter #:coalton/iterator)
   (#:math #:coalton/math)
   (#:mutex #:coalton/threads/mutex)
   (#:par #:coalton/threads/parallel)
   (#:seq #:coalton/seq)
   (#:thread #:coalton/threads/thread)
   (#:vector #:coalton/vector)))

(in-package #:collatz)
```

The timings below were measured in release mode with SBCL 2.6.9 and a 4 GB
heap, on an AMD Ryzen AI Max+ PRO 395, which has 16 cores and 32 hardware
threads. Your
numbers will differ, depending above all on the number of processors of
your machine.

## A First Parallel Loop

This function counts the steps of a Collatz sequence, and the next one
counts the steps of every integer below `n`, storing them in an array:

```lisp
(coalton-toplevel
  (declare collatz-steps (UFix -> UFix))
  (define (collatz-steps n)
    "The number of steps that the Collatz sequence starting at `n` takes to reach 1."
    (rec % ((x n) (steps 0))
      (cond ((<= x 1) steps)
            ((even? x) (% (math:div x 2) (+ steps 1)))
            (True (% (+ (* 3 x) 1) (+ steps 1))))))

  (declare all-steps (UFix -> arr:LispArray UFix))
  (define (all-steps n)
    "The number of Collatz steps of each integer below `n`."
    (let ((steps (arr:make n 0)))
      (for ((i 1 (+ i 1)))
        :while (< i n)
        (arr:set! steps i (collatz-steps i)))
      steps)))
```

To do the same in parallel, replace the `for` loop with `par:for-range!`,
which calls a function on every integer from a start (inclusive) to an
end (exclusive), in parallel:

```lisp
(coalton-toplevel
  (declare parallel-all-steps (UFix -> arr:LispArray UFix))
  (define (parallel-all-steps n)
    "The number of Collatz steps of each integer below `n`, computed in parallel."
    (let ((steps (arr:make n 0)))
      (par:for-range! 1 n
        (fn (i) (arr:set! steps i (collatz-steps i))))
      steps)))
```

```lisp
COLLATZ> (coalton (parallel-all-steps 10))
#(0 0 1 7 2 5 8 16 3 19)
```

For `n` = 10,000,000, `all-steps` takes 1.43 seconds, and
`parallel-all-steps` 0.073 seconds: almost 20 times less.

The function passed to `for-range!` may be called on any worker thread,
in any order, with several calls running at the same time. That is fine
here, because each call sets a different element of the array. Tasks
must not change the *same* data at the same time, though; see [Sharing
Data Between Tasks](#sharing-data-between-tasks).

The worker threads, one per processor available to the process by
default, start the first time that a parallel operation needs them:

```lisp
COLLATZ> (coalton (par:worker-count))
32
```

The environment variable `COALTON_NUM_THREADS` sets the initial number of
workers, and `par:set-worker-count!` changes it.

`for-range!` splits its range in halves, and those in halves, until there
are one or two pieces per worker. Each worker keeps its pending pieces in
a double-ended queue, and a worker that runs out of work steals a piece
from the queue of another. A stolen piece is split again in the same
way. The work thus spreads as finely as needed to keep every worker busy,
while workers that have enough work process large pieces with little
overhead. That is how all processors stay busy until the end, although
some ranges of starting numbers take much longer than others.

## Reductions

A loop that combines values into one, such as a sum, is a *reduction*.
`par:map-reduce-range` computes a function on each integer of a range,
and combines the values with a second function. This computes the total
number of steps of the integers below `n`:

```lisp
(coalton-toplevel
  (declare total-steps (UFix -> UFix))
  (define (total-steps n)
    "The total number of Collatz steps of the integers below `n`."
    (par:map-reduce-range 1 n collatz-steps + 0)))
```

The last argument, `0`, is the result for an empty range. The parallel
reductions require that the combining function be *associative*, and that
this last argument be an *identity* for it, as 0 is for `+`. The values
are combined in the order of the integers, but how they are grouped
depends on how the range is split, which can vary from run to run.

As an example of a combining function other than `+`, this finds the
integer below `n` with the longest Collatz sequence:

```lisp
(coalton-toplevel
  (declare longer (Tuple UFix UFix * Tuple UFix UFix -> Tuple UFix UFix))
  (define (longer a b)
    "Of two pairs of a number and its number of steps, the one with more steps, or `a` if they are equal."
    (if (> (snd b) (snd a)) b a))

  (declare longest-chain (UFix -> Tuple UFix UFix))
  (define (longest-chain n)
    "The integer below `n` with the most Collatz steps, and its number of steps."
    (par:map-reduce-range 2 n
      (fn (i) (Tuple i (collatz-steps i)))
      longer
      (Tuple 1 0))))
```

```lisp
COLLATZ> (coalton (longest-chain 1000000))
#.(TUPLE 837799 524)
```

`longer` is associative, and since it keeps the first of two pairs with
as many steps, the result is the smallest of the integers with the most
steps, as a sequential search would find. `(Tuple 1 0)` is an identity
for `longer` on the pairs that it combines here, because every integer
from 2 up takes at least one step; that is why the range starts at 2.

The integer below 10,000,000 with the longest sequence is 8,400,511, with
685 steps. `longest-chain` finds it in 0.088 seconds, while a sequential
loop, such as `longest-in-range` below, takes 1.49 seconds.

### Chunked Loops

`map-reduce-range` calls its two functions once per integer, and here
each value is a tuple, which must be allocated. When the work per
integer is small, it pays to process the range a chunk at a time instead.
`par:map-reduce-chunks` passes the bounds of each chunk, `lo` (inclusive)
and `hi` (exclusive), to a function, which can then use a tight
sequential loop:

```lisp
(coalton-toplevel
  (declare longest-in-range (UFix * UFix -> Tuple UFix UFix))
  (define (longest-in-range lo hi)
    "The integer from `lo` (inclusive) to `hi` (exclusive) with the most Collatz steps, and its number of steps, or `(Tuple 1 0)` if there is none."
    (rec % ((i lo) (best 1) (best-steps 0))
      (if (>= i hi)
          (Tuple best best-steps)
          (let ((steps (collatz-steps i)))
            (if (> steps best-steps)
                (% (+ i 1) i steps)
                (% (+ i 1) best best-steps))))))

  (declare longest-chain-chunked (UFix -> Tuple UFix UFix))
  (define (longest-chain-chunked n)
    "Like `longest-chain`, but searching chunks of the range with `longest-in-range`."
    (par:map-reduce-chunks 2 n longest-in-range longer (Tuple 1 0))))
```

Each chunk is now searched without allocating, and only one pair per
chunk is combined. This version takes 0.072 seconds, 21 times less than
the sequential `(longest-in-range 2 10000000)`. Likewise, `par:for-chunks!`
is the chunked counterpart of `for-range!`.

### Reproducible Reductions

Floating-point addition is not exactly associative, so a parallel sum of
floating-point numbers can differ in its last digits from one run to the
next, as the grouping of its terms changes. To make it reproducible,
give the reduction a positive `:grain`, as in `(par:map-reduce-range 0 n
f + 0d0 :grain 1000)`. The range is then split into chunks of at most
that many integers, in the same way every time. The other parallel loops
accept `:grain` too.

Finally, `par:fold-map-range` is a reduction that combines values with
`<>`, the operation of a `Monoid`, starting from `mempty`.

## Divide and Conquer

`par:join` calls two functions, potentially in parallel, and returns both
of their values. It is the tool for *divide-and-conquer* algorithms,
which split a problem into subproblems, solve those recursively, and
combine their solutions. The classic example is the Fibonacci function:

```lisp
(coalton-toplevel
  (declare sequential-fib (UFix -> UFix))
  (define (sequential-fib n)
    (if (< n 2)
        n
        (+ (sequential-fib (- n 1)) (sequential-fib (- n 2)))))

  (declare fib (UFix -> UFix))
  (define (fib n)
    (if (< n 25)
        (sequential-fib n)
        (progn
          (let (values a b) = (par:join (fn () (fib (- n 1)))
                                        (fn () (fib (- n 2)))))
          (+ a b)))))
```

`(fib 40)` takes 0.033 seconds, against 0.553 seconds for
`(sequential-fib 40)`: 17 times less.

`join` pushes its second function onto the worker's queue, and calls the
first one. Then, if no other worker has stolen the second function in
the meantime, the worker takes it back and calls it. This makes `join`
cheap: about 25 nanoseconds. If the second function was stolen, the
worker runs other tasks while it waits for the thief to finish it.

Cheap is not free, though. Each `join` also allocates two closures, while
a call of `sequential-fib` does only a few nanoseconds of work of its
own. So `fib` calls `sequential-fib` for arguments below 25: with `join`
at every level of the recursion, it would be slower than
`sequential-fib`. Such a *cutoff* is the usual way to keep tasks large
enough to be worth running in parallel.

## Collections

`par:map` maps a function over a collection in parallel, and returns a
new collection of the same kind. It is the method of the class
`ParallelMap`, which has instances for lists, vectors and `Seq`s:

```lisp
COLLATZ> (coalton (par:map (fn (x) (* x x)) (make-list 1 2 3 4)))
(1 4 9 16)
```

`par:sort!` sorts a vector in place, with a stable parallel merge sort,
and `par:sort-by!` does the same with a given less-than function.
Together:

```lisp
(coalton-toplevel
  (declare sorted-steps (UFix -> Vector UFix))
  (define (sorted-steps n)
    "The numbers of Collatz steps of the integers below `n`, in increasing order."
    (let ((steps (par:map collatz-steps (the (Vector UFix) (into (range 1 (- n 1)))))))
      (par:sort! steps)
      steps)))
```

```lisp
COLLATZ> (coalton (sorted-steps 20))
#(0 1 2 3 4 5 6 7 8 9 9 12 14 16 17 17 19 20 20)
```

`par:map-reduce` and `par:fold-map` are reductions over the elements of
a collection, and `par:for-each!` calls a function on every element of a
collection. They work on `Seq`s, and on any collection with an instance
of the class `RandomAccess`, such as `Vector` or `LispArray`: these are
the instances of the class `ParallelFoldable`. For example, this finds
the number in a `Seq` with the longest Collatz sequence:

```lisp
(coalton-toplevel
  (declare longest-chain-in (seq:Seq UFix -> Tuple UFix UFix))
  (define (longest-chain-in numbers)
    "The element of `numbers` with the most Collatz steps, and its number of steps."
    (par:map-reduce (fn (i) (Tuple i (collatz-steps i))) longer (Tuple 1 0) numbers)))
```

```lisp
COLLATZ> (coalton (longest-chain-in (into (the (List UFix) (range 2 999)))))
#.(TUPLE 871 178)
```

Finally, `par:map-into!` stores the values of a function on the elements
of one `RandomAccess` collection into another.

## Scopes

`join` divides work in two. When work divides into a varying number of
pieces, or when the pieces only appear as the computation proceeds, use a
*scope*. `par:scope` calls a function with a new scope. In it,
`par:spawn` starts tasks, which may spawn more tasks in the same scope,
and `scope` returns once all of them have finished.

For example, suppose that work comes as a tree, with ranges of integers
at its leaves. `split-work` makes such a tree, splitting a range into a
quarter, another quarter and a half, until the pieces are small. The
leaves of this tree lie at varying depths:

```lisp
(coalton-toplevel
  (define-type Work
    "Work to do: a chunk of integers, from `lo` (inclusive) to `hi` (exclusive), or several parts."
    (Chunk UFix UFix)
    (Parts (List Work)))

  (declare split-work (UFix * UFix -> Work))
  (define (split-work lo hi)
    "The integers from `lo` to `hi`, split unevenly into chunks of fewer than 10,000 integers."
    (if (< (- hi lo) 10000)
        (Chunk lo hi)
        (let ((a (+ lo (math:div (- hi lo) 4)))
              (b (+ lo (math:div (- hi lo) 2))))
          (Parts (make-list (split-work lo a)
                            (split-work a b)
                            (split-work b hi)))))))
```

This computes the total number of steps of the integers in such a tree,
with a task for each part of every node:

```lisp
(coalton-toplevel
  (declare steps-in-range (UFix * UFix -> UFix))
  (define (steps-in-range lo hi)
    "The total number of Collatz steps of the integers from `lo` (inclusive) to `hi` (exclusive)."
    (rec % ((i lo) (total 0))
      (if (>= i hi)
          total
          (% (+ i 1) (+ total (collatz-steps i))))))

  (declare work-steps (Work -> UFix))
  (define (work-steps work)
    "The total number of Collatz steps of the integers in `work`, visiting its parts in parallel."
    (let ((total (atomic:new 0)))
      (par:scope
       (fn (s)
         (let ((visit (fn (w)
                        (match w
                          ((Chunk lo hi)
                           (let ((steps (steps-in-range lo hi)))
                             (atomic:update! (fn (sum) (+ sum steps)) total)
                             (values)))
                          ((Parts pieces)
                           (iter:for-each! (fn (piece)
                                             (par:spawn s (fn () (visit piece))))
                                           (iter:into-iter pieces)))))))
           (visit work))))
      (atomic:read total))))
```

`visit`, a local function that calls itself (`let` binds recursively),
spawns a task to visit each part of a node, so that the tasks fan out
over the workers as the tree is explored. Each chunk adds its total to an
atomic cell, which tasks can update safely at the same time (see [Sharing
Data Between Tasks](#sharing-data-between-tasks)). On the tree
`(split-work 1 10000000)`, `work-steps` takes 0.071 seconds, 20 times
less than `(steps-in-range 1 10000000)`.

## Futures

`par:async` starts a computation in parallel and returns a *future* for
its value, and `par:await` waits for the computation to finish and
returns its value. This runs two computations of the previous sections
at the same time:

```lisp
(coalton-toplevel
  (declare collatz-report (UFix -> Tuple (Tuple UFix UFix) UFix))
  (define (collatz-report n)
    "The integer below `n` with the most Collatz steps, with its number of steps, and the total number of steps of the integers below `n`."
    (let ((longest (par:async (fn () (longest-chain-chunked n))))
          (total (par:async (fn () (total-steps n)))))
      (Tuple (par:await longest) (par:await total)))))
```

```lisp
COLLATZ> (coalton (collatz-report 1000000))
#.(TUPLE #.(TUPLE 837799 524) 131434272)
```

A future may be awaited any number of times, by any thread, and
`par:done?` tells whether its computation has finished. Futures suit
independent computations, like the two parts of this report. For
divide-and-conquer algorithms, prefer `join` and scopes: a worker that
waits for a `join` or a scope can run the tasks that the computation it
waits for has created, but a worker that awaits a future running on
another worker can only run tasks of its own computation in the
meantime, and may sit idle.

## Errors

An exception thrown in a task is thrown again by the operation that waits
for the task: `join`, `await`, the end of a scope, or the parallel loop,
reduction or map that ran the task. So `catch` works around parallel
operations as it does around sequential code. Here, the function passed
to `par:map` throws an exception on zero:

```lisp
(coalton-toplevel
  (define-exception InvalidStart
    (InvalidStart UFix))

  (declare steps-of (List UFix -> List UFix))
  (define (steps-of numbers)
    "The number of Collatz steps of each of `numbers`, which must be positive."
    (par:map (fn (n)
               (if (== n 0)
                   (throw (InvalidStart n))
                   (collatz-steps n)))
             numbers)))
```

```lisp
COLLATZ> (coalton (catch (Ok (steps-of (make-list 7 0 27)))
                    ((InvalidStart n) (Err n))))
#.(ERR 0)

COLLATZ> (coalton (catch (Ok (steps-of (make-list 7 1 27)))
                    ((InvalidStart n) (Err n))))
#.(OK (16 0 111))
```

Other errors that a task signals, such as the panics signaled by `error`,
`unwrap` and failed `assert`s, propagate in the same way, and
`result:try` works around parallel operations too. If several tasks
fail, the operation throws the exception of one of them.

An operation throws the exception of a task only once all of its tasks
have finished or been cancelled: no task of a `join`, a scope or a
parallel loop keeps running after the operation exits, normally or not.
So the cleanup forms of a `protect` around a parallel operation run after
its tasks are done. A future, on the other hand, keeps running when a
thread awaiting it is unwound.

When debugging in the REPL, evaluate `(setf
coalton/threads/runtime:*debug-tasks* t)`: an error in a task then
enters the debugger in the worker thread that ran the task, before it is
transferred to the waiting thread.

### Handlers and Resumptions

A task runs either on the thread that started the parallel operation, or
on another worker thread. On another thread, it does not see the dynamic
variable bindings, `handle` branches or resumptions established around
the operation, and a `handle` around the operation sees the task's
exceptions only after the task has unwound, when the operation throws
them again. So such a `handle` cannot reliably resume a task with
`resume-to`. Handle the exceptions that should resume a task within the
task instead, as `steps-or-zero` does here:

```lisp
(coalton-toplevel
  (define-resumption (UseSteps UFix)
    "Continue with the given number of steps.")

  (declare checked-steps (UFix -> UFix))
  (define (checked-steps n)
    "The number of Collatz steps of `n`. On zero, throw `InvalidStart`, which can be resumed with `UseSteps`."
    (resumable (if (== n 0)
                   (throw (InvalidStart n))
                   (collatz-steps n))
      ((UseSteps steps) steps)))

  (declare steps-or-zero (List UFix -> List UFix))
  (define (steps-or-zero numbers)
    "The number of Collatz steps of each of `numbers`, or 0 for zero."
    (par:map (fn (n)
               (handle (checked-steps n)
                 ((InvalidStart _) (resume-to (UseSteps 0)))))
             numbers)))
```

```lisp
COLLATZ> (coalton (steps-or-zero (make-list 7 0 27)))
(16 0 111)
```

Had `steps-or-zero` put its `handle` around `par:map`, `resume-to` would
signal a `ControlError` whenever the task that threw ran on another
thread, because the task's `resumable` would already have been unwound.
Likewise, a task must not `resume-to` a resumption established around
the operation.

## Sharing Data Between Tasks

Nothing prevents tasks from changing the same data at the same time, with
unpredictable results. Tasks may set distinct elements of the same array
or vector, as in `parallel-all-steps`, but must not change the structure
of a collection, for example with `vector:push!`, while other tasks
access it. The elements of a `LispArray Bit` are packed into machine
words, so tasks must not even set elements in the same block of 64
(indices `64k` to `64k + 63`) at the same time; `par:map-into!` takes
care of this. To share mutable state between tasks, use an atomic cell,
a mutex or a channel.

An *atomic* cell, from `coalton/threads/atomic`, can be updated by
several tasks at the same time. This counts the integers below `n` whose
sequences take more than `threshold` steps:

```lisp
(coalton-toplevel
  (declare count-long-chains (UFix * UFix -> UFix))
  (define (count-long-chains n threshold)
    "The number of integers below `n` that take more than `threshold` steps."
    (let ((count (atomic:new 0)))
      (par:for-range! 1 n
        (fn (i)
          (when (> (collatz-steps i) threshold)
            (atomic:increment! count)
            (values))))
      (atomic:read count))))
```

```lisp
COLLATZ> (coalton (count-long-chains 10000000 300))
191241
```

A reduction does the same without any shared state:

```lisp
(coalton-toplevel
  (declare count-long-chains-by-reduction (UFix * UFix -> UFix))
  (define (count-long-chains-by-reduction n threshold)
    "Like `count-long-chains`, but with a reduction."
    (par:map-reduce-range 1 n
      (fn (i) (if (> (collatz-steps i) threshold) 1 0))
      + 0)))
```

Few integers take more than 300 steps, so the atomic counter is rarely
updated, and costs little: `count-long-chains` takes 0.09 seconds, and
the reduction 0.07 seconds. But when every iteration updates the same
cell, the workers take turns at its location in memory, and the updates
run one at a time:

```lisp
(coalton-toplevel
  (declare total-steps-with-atomic (UFix -> UFix))
  (define (total-steps-with-atomic n)
    "Like `total-steps`, but adding up the steps in an atomic cell. Don't do this!"
    (let ((total (atomic:new 0)))
      (par:for-range! 1 n
        (fn (i)
          (let ((steps (collatz-steps i)))
            (atomic:update! (fn (sum) (+ sum steps)) total)
            (values))))
      (atomic:read total))))
```

For `n` = 10,000,000, this takes 3.5 seconds: 47 times as long as
`total-steps`, and even 2.4 times as long as the sequential
`(steps-in-range 1 n)`. Prefer reductions to shared counters, and when a
cell must be shared, update it rarely, as `work-steps` does, once per
chunk of up to 10,000 integers.

A *mutex*, from `coalton/threads/mutex`, lets only one task at a time run
a piece of code. This collects the integers with long sequences in a
vector:

```lisp
(coalton-toplevel
  (declare long-chains (UFix * UFix -> Vector UFix))
  (define (long-chains n threshold)
    "The integers below `n` that take more than `threshold` steps, in no particular order."
    (let ((found (vector:new))
          (lock (mutex:new)))
      (par:for-range! 1 n
        (fn (i)
          (when (> (collatz-steps i) threshold)
            (mutex:with-lock lock
              (fn ()
                (vector:push! i found)
                (values))))))
      found)))
```

```lisp
COLLATZ> (coalton (vector:length (long-chains 10000000 300)))
191241
```

The order of the integers in the vector is unpredictable, since tasks
push them as they find them.

A few more rules keep parallel programs correct:

- Do not hold a mutex while calling a parallel operation. A worker that
  waits inside `join`, `await` or a scope runs other tasks in the
  meantime, and one of them could try to acquire the same mutex.
- A task must not wait for another task, except with `join`, `await` or
  the end of a scope; otherwise, the program can deadlock.
- A `for` loop updates its variables in place, so a task created in the
  body of a `for` loop that refers to a loop variable may see a later
  value of it. Bind the value with `let` first.

## Threads and Channels

The pool of workers is meant for computation. For activities that wait,
such as input and output, or that last as long as the program, start an
operating-system thread with `thread:spawn`. A thread is typed by the
value of its function, which `thread:join` waits for and returns; if an
error ended the thread, `thread:join` signals it again.

Threads can communicate through channels, from `coalton/threads/channel`:
`channel:send!` adds a value to a channel, and `channel:recv!` waits for a
value and removes it. Here, a thread adds up the numbers it receives,
until it receives `None`:

```lisp
(coalton-toplevel
  (declare sum-in-background (UFix -> UFix))
  (define (sum-in-background n)
    "Add up the integers below `n` in another thread, which receives them through a channel."
    (let ((numbers (channel:new)))
      (let ((adder (thread:spawn
                    (fn ()
                      (rec % ((total 0))
                        (match (channel:recv! numbers)
                          ((Some x) (% (+ total x)))
                          ((None) total)))))))
        (for ((i 0 (+ i 1)))
          :repeat n
          (channel:send! numbers (Some i)))
        (channel:send! numbers None)
        (thread:join adder)))))
```

```lisp
COLLATZ> (coalton (sum-in-background 1000))
499500
```

The packages `coalton/threads/condition-variable` and
`coalton/threads/semaphore` provide the other classic synchronization
tools.

## Performance

How much faster a parallel program runs depends on more than the number
of processors:

- **Granularity.** Parallelism pays when tasks do at least a few
  microseconds of work. Use cutoffs in recursive algorithms, as in `fib`,
  and chunked loops when the iterations of a loop are cheap.
- **Allocation.** SBCL's garbage collector stops all threads while it
  works, so code that allocates heavily gains less from parallelism.
  Unboxed arrays, such as `LispArray F64`, and chunked loops with unboxed
  accumulators help.
- **Memory bandwidth.** Loops that do little arithmetic for each value
  they read from memory are limited by the bandwidth of the memory, which
  a few processors can use up.
- **Contention.** Tasks that update the same atomic cell, or take the
  same mutex, wait for each other. Reductions don't.
- **Blocking.** A task that blocks, on a mutex, a channel or the end of a
  thread, keeps its worker from running other tasks.
- **Workers.** On processors with several hardware threads per core, code
  limited by allocation may run faster with one worker per core. Try it
  with `COALTON_NUM_THREADS`.

The [parallel numerics
examples](https://github.com/coalton-lang/coalton/tree/main/examples/parallel-numerics)
show these effects. On the machine above:

| Example | Speedup | Limited by |
| --- | ---: | --- |
| Ising model, 2048×2048 lattice, 200 sweeps | 16.0x | computation |
| N-body, 8192 bodies, 10 steps | 13.9x | computation |
| FDTD electromagnetics, 1024×1024 grid, 1000 steps | 5.2x | memory bandwidth |
| Path tracing, 640×360 pixels, 16 samples per pixel | 3.1x | garbage collection |

With 16 workers, one per core, the path tracer's speedup rises to 4.4x.

## Further Reading

- The [`coalton/threads`
  README](https://github.com/coalton-lang/coalton/tree/main/threads)
  details the guidelines and caveats of this tutorial, and describes how
  the runtime works.
- The [reference](/reference/#coalton-threads-parallel-package) documents
  `coalton/threads/parallel` and the library's other packages.
- The [parallel numerics
  examples](https://github.com/coalton-lang/coalton/tree/main/examples/parallel-numerics)
  are complete parallel programs: an FDTD electromagnetic simulation, an
  N-body simulation, a Monte Carlo simulation of the Ising model, and a
  path tracer.
- The system `coalton/threads/benchmarks` measures the speedups of the
  parallel operations on your machine. Load it, and evaluate
  `(coalton/threads/benchmarks:run-benchmarks)`.
