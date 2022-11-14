
(defpackage :functional-cl
  (:nicknames :functional :f)
  (:use #:cl)
  (:export 
  
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
  #:rot-paramsr
  #:rot-paramsr-c
  #:rot-paramsl
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

  ))