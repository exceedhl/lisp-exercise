(require :lisp-unit)

(defpackage #:l99-tests
  (:use #:common-lisp #:lisp-unit #:l99))

(in-package #:l99-tests)

(define-test test-my-last
  (assert-equal '(d) (my-last '(a b c d)))
  (assert-equal '() (my-last nil))
  (assert-equal '(a) (my-last '(a))))

(define-test test-my-but-last
  (assert-equal '(c d) (my-but-last '(a b c d)))
  (assert-equal '(a b) (my-but-last '(a b)))
  (assert-equal '(a) (my-but-last '(a)))
  (assert-equal nil (my-but-last nil)))

(define-test test-my-length
  (assert-equal 0 (my-length nil))
  (assert-equal 1 (my-length '(a)))
  (assert-equal 3 (my-length '(a b c))))

(define-test test-my-element-at
  (assert-equal 'a (my-element-at '(a) 1))
  (assert-equal 'b (my-element-at '(a b c) 2))
  (assert-equal 'c (my-element-at '(a b c) 3))
  (assert-error 'simple-type-error (my-element-at 0 '(a b c))))

(define-test test-my-reverse
  (assert-equal '(c b a) (my-reverse '(a b c)))
  (assert-equal nil (my-reverse nil)))

(define-test test-my-palindrome
  (assert-true (my-palindrome '(a a)))
  (assert-false (my-palindrome '(a b)))
  (assert-true (my-palindrome '(a b c b a))))

(define-test test-my-flatten
  (assert-equal '(a b c d e) (my-flatten '(a (b (c d) e))))
  (assert-equal '(a b c d e) (my-flatten '((a b) (c (d e)))))
  (assert-equal nil (my-flatten nil))
  (assert-equal '(nil a b c nil) (my-flatten '(nil (a b (c nil))))))

(define-test test-compress
  (assert-equal '(a b c a d e) (compress '(a a a a b c c a a d e e e e)))
  (assert-equal '(1 2 3 4) (compress '(1 1 2 3 3 4)))
  (assert-equal nil (compress nil)))

(define-test test-my-pack
  (assert-equal '((a a a a) (b) (c c) (a a) (d) (e e e e))
		(my-pack '(a a a a b c c a a d e e e e)))
  (assert-equal '((a)) (my-pack '(a)))
  (assert-equal '((a a)) (my-pack '(a a)))
  (assert-equal nil (my-pack nil)))

(run-tests)
