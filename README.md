# Functional-cl

The Functional-cl library provides elementary utility functions for implementing Common Lisp software in a functional programming style. 

This library was developed by M. Zala as part of a bachelor's thesis at Zurich University of Applied Sciences in 2022. 

## Overview

Common Lisp is one of the languages that allow and encourage the functional programming style: The concepts of first-class functions and lambda functions have been part of the language since the beginning. Various aspects of functional programming have already been implemented in other CL-libraries (see [Other functional libraries](#other-functional-libraries)). However, none of these libraries offers a well-rounded collection of elementary auxiliary functions like currying or parameter handling. This is where Functional-cl comes into play.

The main areas covered are covered:
  - currying / partial application
  - function composition
  - curried mathematic operators
  - utility functions for the regular list data structure
  - parameter handling
  - memoization

Almost every function in this library has a curried version, which is generated at compile time by macros. They keep the same function name with the suffix "-c". E.g. ```take``` => ```take-c```.

The first version of this project was developed in the context of a lecture on programming paradigms at the Zurich University of Applied Sciences (ZHAW). It was revised, extended and packaged as part of a bachelor's thesis.

Many functions of this library are inspired by the JavaScript libraries [Lemonad](https://github.com/fogus/lemonad) and [Ramda](https://github.com/ramda/ramda).

## Install

_Note_: Since Common Lisp development is usually done on Unix-like operating systems, there are no installation instructions provided for Windows.

Clone the repo into your ASDF registry, where ASDF searches for system definitions. By the default configuration ASDF searches the subdirectories of: 
  
  - `~/.local/share/common-lisp/source`
  - `~/common-lisp`

If none of the above directories exists you may want to create one of them.

Alternatively you can also clone the repository to a different directory and then create a symlink to the .asd file inside your registry.

```console 
$ ln -s $(pwd)/functional-cl.asd <path to ASDF registry>
```

Now the library can be used in the REPL or in source files:

```lisp
(require :functional-cl)
```

## Currently Available Functions

```lisp
  ;;; COMPOSTITION
  #:compose-simple
  #:compose-simple-c
  #:compose-funcs
  #:compose-funcs-c
  #:pipeline
  #:and-checker
  #:or-checker
  #:dispatch
  #:decorate
  #:decorate-c
  #:defaults
  #:constrained
  #:constrained-c
  #:keep-args
  #:keep-args-c
  #:tap

  ;;; CURRIED-MATH
  #:add-c
  #:sub-c
  #:mul-c
  #:div-c
  #:pow-c
  #:log-c
  #:gt-c
  #:gte-c
  #:lt-c
  #:lte-c
  #:eq-c
  #:neq-c

  ;;; CURRY
  #:partial
  #:partialr
  #:curry2
  #:curry3
  #:curry2r
  #:curry3r
  #:curry-n
  #:curry
  #:uncurry2
  #:uncurry3
  #:uncurry2r
  #:uncurry3r
  #:uncurry-n

  ;;; ITERATION
  #:repeatedly
  #:repeatedly-c
  #:iterate
  #:iterate-c
  #:iterate-while
  #:iterate-while-c
  #:n-times-in-list
  #:n-times-in-list-c
  #:range
  #:range-c

  ;;; LIST
  #:zip-to-alist
  #:zip-to-alist-c
  #:flatten
  #:flatten-c
  #:transpose
  #:partition
  #:partition-c
  #:apply-to-partlist
  #:apply-to-partlist-c
  #:rot-left
  #:rot-left-c
  #:rot-right
  #:rot-right-c
  #:swap
  #:swap-c
  #:copy-list-deep
  #:split-at
  #:split-at-c
  #:insert-at
  #:insert-at-c
  #:replace-at
  #:replace-at-c
  #:mod-at
  #:mod-at-c
  #:remove-at
  #:remove-at-c
  #:slice
  #:slice-c
  #:lsort
  #:lsort-c
  #:take
  #:take-c
  #:back

  ;;; PARAMETER
  #:splat
  #:unsplat
  #:switch-params
  #:switch-params-c
  #:args
  #:args-c
  #:rot-paramsrel
  #:rot-paramsl-c

  ;;; STRUCT
  #:change-slot
  #:change-slot-c

  ;;; TRANSFORM 
  #:flat-map
  #:flat-map-c
  #:map-list
  #:map-list-c
  #:reduce-list-left
  #:reduce-list-left-c
  #:reduce-list-right
  #:reduce-list-right-c
  #:filter-list
  #:filter-list-c
  #:reduce-list
  #:reduce-list-c
  #:foldl
  #:foldl-c
  #:foldr
  #:foldr-c
  #:map-index
  #:map-index-c

  ;;; UTIL
  #:setfun
  #:memoize
```

## Other Functional Libraries

The following is a list of other functional libraries for Common Lisp:

### Utility Functions
- [folio 2](https://github.com/mikelevins/folio2): A collection of small libraries that provide support for functional concepts
- [Alexandria](https://gitlab.common-lisp.net/alexandria/alexandria)'s file _alexandira-1/function.lisp_: Basic support for currying
- [Cl-celwk](https://github.com/Soul-Clinic/cl-celwk)'s file _functional.lisp_: A small collection of functional auxiliary functions 
- [Modf](https://github.com/smithzvk/modf): A functional version of the macro _setf_ without side effects
- [F-underscore](https://gitlab.common-lisp.net/bpm/f-underscore): Lambda shorthand macros

### Functional (Immutable) Data Structures
- [FSet](https://github.com/slburson/fset): The classic
- [Sycamore](https://github.com/ndantam/sycamore): A more performance optimized library

### Generators (Lazy Sequences)
- [Series](https://github.com/tokenrove/series): The classic
- [GTWIWTG](https://github.com/d3v3l0/gtwiwtg): "Generators The Way I Want Them Generated" project
- [Generator](https://github.com/gefjon/generator): A reinvention of the classic _Series_
- [Snakes](https://github.com/BnMcGn/snakes): Python-style generators

### Pattern Matching
- [Optima](https://github.com/m2ym/optima): The classic
- [Trivia](https://github.com/guicho271828/trivia): A backward compatible successor of the classic _Optima_

### Lazy Evaluation
- [Clazy](https://gitlab.common-lisp.net/clazy/clazy): Macros for lazy calling

### Monads
- [Cl-monad-macros](https://cl-monad-macros.common-lisp.dev/monad-macros.htm): Macros for creating haskell-style monads 
