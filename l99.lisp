(defpackage #:l99
  (:use #:common-lisp)
  (:export #:my-last #:my-but-last #:my-length #:my-element-at
	   #:my-reverse #:my-palindrome #:my-flatten #:compress
	   #:my-pack))
  
(in-package :l99)

(defun my-last (lst)
  (labels ((rec (lst result)
	     (if (null lst)
		 result
		 (rec (cdr lst) lst))))
    (rec lst lst)))

(defun my-but-last (lst)
  (labels ((rec (lst length)
	     (if (<= length 2)
		 lst
		 (rec (cdr lst) (1- length)))))
    (rec lst (length lst))))

(defun my-length (lst)
  (labels ((rec (lst length)
	     (if (null lst)
		 length
		 (rec (cdr lst) (1+ length)))))
    (rec lst 0)))

(defun my-element-at (lst n)
  (if (< n 1)
      (error "Index out of bounds"))
  (labels ((rec (lst pos)
	     (if (= pos n)
		 (car lst)
		 (rec (cdr lst) (1+ pos)))))
    (rec lst 1)))

(defun my-reverse (lst)
  (labels ((rec (lst acc)
	     (if (null lst)
		 acc
		 (rec (cdr lst) (cons (car lst) acc)))))
    (rec lst nil)))

(defun my-palindrome (lst)
  (equal lst (my-reverse lst)))

(defun my-flatten (tree)
  (labels ((rec (tree acc)
	     (if (null tree)
		 acc
		 (if (atom (car tree))
		     (rec (cdr tree) (cons (car tree) acc))
		     (rec (cdr tree) (append (rec (car tree) nil) acc))))))
    (nreverse (rec tree nil))))

(defun compress (lst)
  (labels ((rec (lst acc)
	     (if (null lst)
		 (nreverse acc)
		 (let ((first (car lst)))
		   (if (eql first (car acc))
		       (rec (cdr lst) acc)
		       (rec (cdr lst) (cons first acc)))))))
    (rec lst nil)))

(defun my-pack (lst)
  (labels ((get-same (lst acc)
	     (cond
	       ((or (null acc) (eql (car lst) (car acc)))
		(get-same (cdr lst) (cons (car lst) acc)))
	       (t (values acc lst))))
	   (rec (lst acc)
	     (if (null lst)
		 (nreverse acc)
		 (multiple-value-bind (sublist rest) (get-same lst nil)
		   (rec rest (cons sublist acc))))))
    (rec lst nil)))
