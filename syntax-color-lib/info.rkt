#lang info

(define collection 'multi)

(define deps '("base"
               "parser-tools-lib"
               "option-contract-lib"))

(define build-deps '("rackunit-lib"))

(define pkg-desc "implementation (no documentation) part of \"syntax-color\"")

(define pkg-authors '(mflatt))

(define version "1.6")

(define license
  '(Apache-2.0 OR MIT))
