(defpackage #:earley-parser
  (:use "COMMON-LISP" "CL-USER")
  (:export
   #:chart-listing->trees
;;   *debug*
;;   *string-comparer*
   #:earley-parse
   #:load-lexicon
   #:load-bnf-grammar))

(in-package :earley-parser)

(defparameter *debug* 0)
(defparameter *string-comparer* #'equalp "equalp = case insensitive")
(defparameter *whitespace* (list #\Newline #\Space #\Tab))
