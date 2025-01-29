# CS 6120 Lesson 2: Representing Programs 

This repo contains my OCaml implementation of the tasks for [Lesson 2](https://www.cs.cornell.edu/courses/cs6120/2025sp/lesson/2//#tasks).

## Code overview:
- [syntax.ml](./lib/syntax.ml): Type definitions for Bril programs
- [cfg.ml](./lib/cfg.ml): The algorithm for forming basic blocks & building control flow graphs 
  (translated from the Python code discussed in-class)
- [helpers.ml](./lib/syntax.ml): Helper functions for dealing with JSON 

## Example:
To test the CFG algorithm on an example Bril program [`jmp.bril`](./bril_files/jmp.bril), run the following:
```bash 
$ bril2json < bril_files/jmp.bril | dune exec -- main | dot -Tpdf -o cfg.pdf
$ open cfg.pdf
```
This produces a GraphViz visualization of the CFG in [cfg.pdf](./cfg.pdf).

On the example program, the basic blocks built are: 
```bash
# Basic blocks for `jmp.bril`
b0
  ((Const (v TyInt) (LitInt 4)) (Jmp somewhere))
b1
  ((Const (v TyInt) (LitInt 2)))
somewhere
  ((Print v))
```
Note that the OCaml implementation has found the same basic blocks as the Python program discussed in the [pre-recorded lesson 2 video](https://www.cs.cornell.edu/courses/cs6120/2025sp/lesson/2//#tasks)! This gives us some confidence that the OCaml implementation behaves the same as the Python one.


The [`bril_files`](./bril_files/) subfolder contains a bunch of example Bril files (taken from the test cases in the [main Bril repo](https://github.com/sampsyo/bril)).

## Building the code
- This repo compiles with OCaml 5.0.0 or newer.
- To run the code, the [Opam](https://opam.ocaml.org) package manager is required. Installation instructions for Opam can be found [here](https://opam.ocaml.org/doc/Install.html).
- Run `make install` to install all OCaml dependencies
- Run `make` to compile (under the hood, this invokes the [Dune](https://dune.build/install) build system)
