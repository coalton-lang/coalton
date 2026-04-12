---
title: "Introduction and Getting Started"
hideMeta: true
weight: 10
---

Coalton is a statically typed functional language embedded in Common Lisp. Since
it's embedded in Lisp, it uses the ordinary Common Lisp toolchain. Despite that,
Coalton is a full-fledged programming language with an optimizing compiler.

Here, we assume you'll adopt a standard Lisp development system, which uses Emacs
and SLIME. *These are not strictly necessary, and make the learning curve steeper,
but improve the development experience substantially.*

## Install SBCL

Figure out a way to install SBCL on your platform. In the worst case, you can
download binaries from the [SBCL website](https://www.sbcl.org/platform-table.html).
We highly recommend having a recent version of SBCL. If the binary available to you
is old, then after installing it, compiling a new SBCL is very easy:

```sh
git clone git://git.code.sf.net/p/sbcl/sbcl
cd sbcl
sh make.sh --fancy
sh install.sh
```

## Install Quicklisp

Quicklisp is a package manager for Common Lisp. It will make acquiring dependencies
easier. Instructions can be found on the [Quicklisp website](https://www.quicklisp.org/beta/),
but the essentials are as follows:

```
curl -O https://beta.quicklisp.org/quicklisp.lisp
sbcl --load quicklisp.lisp
(quicklisp-quickstart:install)
(ql:add-to-init-file)
```

## Install Emacs

Emacs is available pretty much everywhere, for every operating system, in every
package manager.

## Download and Load Coalton

Coalton should be downloaded into a place Lisp can see it. Quicklisp provides
a handy place in `~/quicklisp/local-projects/`:

```
cd ~/quicklisp/local-projects/
git clone https://github.com/coalton-lang/coalton.git
```

At this point, you should be able to load Coalton:

```
sbcl
(ql:quickload "coalton")
```

If you run into an error, you'll see a list of options. Press Ctrl+D to exit.

## Setting up Emacs with SLIME

This document isn't an Emacs tutorial. There are lots of those online, and one
built in to Emacs. This document will tell you what you need to get Coalton
setup with Emacs.

SLIME is an extension to Emacs providing IDE-like functionality, including
a REPL, jump-to-definition, etc. Fortunately, it can be installed with Emacs's
native package manager.

1. Start Emacs.
2. Update the package list: Press `Alt+x` and type `package-list-packages`.
3. Install SLIME: Press `Alt-x`, type `package-install`, and type `slime`.

Now open your `~/.emacs` file and add the following. Be sure to replace
the `sbcl` path with the correct one on the line with `; ***`.

```lisp
(show-paren-mode 1)

(slime-setup '(slime-fancy
               slime-company
               slime-coalton))

(setq lisp-lambda-list-keyword-parameter-alignment t
      lisp-lambda-list-keyword-alignment t
      lisp-align-keywords-in-calls t)

(put 'make-instance 'common-lisp-indent-function 1)

(setq slime-lisp-implementations
      '((sbcl ("/YOUR/PATH/TO/sbcl") :coding-system utf-8-unix))) ; ***

(add-to-list 'auto-mode-alist '("\\.ct$" . lisp-mode))
```

Save and restart Emacs.

## A First Coalton Program

Let's make a simple Coalton program.

```
cd ~/quicklisp/local-projects
mkdir first-coalton
cd first-coalton
touch first-coalton.asd
touch hello.ct
```

Now open Emacs. Open `first-coalton.asd` and define this project's build
file:

```lisp
;;;; first-coalton.asd

(defsystem "first-coalton"
  :defsystem-depends-on ("coalton-asdf")
  :depends-on ("coalton")
  :serial t
  :components ((:ct-file "hello")))
```

Save that file, and open `hello.ct`:

```lisp
(defpackage #:first-coalton/hello
  (:local-nicknames (#:str #:coalton/string))
  (:use #:coalton #:coalton-prelude)
  (:export #:hello))

(in-package #:first-coalton/hello)

(coalton-toplevel
  (declare hello (String -> String))
  (define (hello name)
    (str:concat "Hello " name)))
```

Save that file. Now press `Alt+x` and type `slime`. A REPL should start.
(If one doesn't, either your SBCL is broken or SLIME wasn't installed.)
Load your project with `(asdf:load-system "first-coalton")`. Once everything
loads, you can now run your function:

```
CL-USER> (in-package #:first-coalton/hello)
FIRST-COALTON/HELLO> (coalton (hello "Coalton"))
"Hello Coalton"
```

Now you have all the tools you need to start hacking Coalton.
