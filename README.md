# 48 Scheme

A small Scheme interpreter based on [Write Yourself a Scheme in 48 Hours](http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours). Still working on the exercises in the book.

[![Build Status](https://travis-ci.org/shuhei/48scheme.png)](https://travis-ci.org/shuhei/48scheme)

## Build & Play

```
$ cabal install --only-dependencies
$ cabal configure
$ cabal build
$ ./dist/build/48scheme/48scheme
Lisp>>> (define (factorial n) (if (< n 2) 1 (* n (factorial (- n 1)))))
(lambda ("n") ...)
Lisp>>> (factorial 10)
3628800
Lisp>>> quit
$
```

## Test

```
$ cabal install --only-dependenecies --enable-tests
$ cabal configure --enable-tests
$ cabal build
$ cabal test
```
