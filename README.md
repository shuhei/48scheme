# 48 Scheme

A small Scheme interpreter based on [Write Yourself a Scheme in 48 Hours](http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours). Still working on the exercises in the book.

[![Build Status](https://travis-ci.org/shuhei/48scheme.png)](https://travis-ci.org/shuhei/48scheme)

## Installation

Prerequisite:

- [stack](https://github.com/commercialhaskell/stack)

```
$ stack setup
```

## Build & Play

```
$ stack build
$ stack exec lisp
Lisp>>> (define (factorial n) (if (< n 2) 1 (* n (factorial (- n 1)))))
(lambda ("n") ...)
Lisp>>> (factorial 10)
3628800
Lisp>>> quit
$
```

## Test

```
$ stack test
```
