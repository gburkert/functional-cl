(defsystem "functional-cl"
  :author "Gerrit Burkert <gerrit.burkert@zhaw.ch> & Marco Zala <marco.e.zala@gmail.com>"
  :version "0.0.1"
  :licence "MIT"
  :description "Functional-cl provides elementary utility functions for functional programming."
  :long-description 
  "Common Lisp is one of the languages that allow and encourage the functional programming style: 
The concepts of first-class functions and lambda functions have been part of the language since the
beginning. Various aspects of functional programming have already been implemented in other 
CL-libraries. However, none of these libraries offers a well-rounded collection of elementary
auxiliary functions like currying or parameter handling. This is where Functional-cl comes into
play.

The main areas covered are covered:
  - currying / partial application
  - function composition
  - curried mathematic operators
  - utility functions for the regular list data structure
  - parameter handling
  - memoization

Almost every function in this library has a curried version, which is generated at compile time by 
macros. They keep the same function name with the suffix '-c'. E.g. take => take-c.

The first version of this project was developed in the context of a lecture on programming paradigms
at the Zurich University of Applied Sciences (ZHAW). It was revised, extended and packaged as part
of a bachelor's thesis.

Many functions of this library are inspired by the JavaScript libraries Lemonad and Ramda."
  :pathname "src/"
  :serial t
  :components ((:file "package")
               (:file "curry")
               (:file "util")
               (:file "parameter")
               (:file "curried-math")
               (:file "composition")
               (:file "transform")
               (:file "iteration")
               (:file "list")
               (:file "struct")))
