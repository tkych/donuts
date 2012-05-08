;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-
;;;     Last Updated : 2012/05/09 07:50:02 tkych

;; cl-utils for donuts

;;====================================================================
;; CL-Utils
;;====================================================================
(in-package :in-donuts)

;;; ^ -> lambda
;;; !!! Don't use 1st place. EX. ((^ (x) (1+ x)) 3) => error !!!
;;;                              (fc (^ (x) (1+ x)) 3) => 4
(defmacro ^ (lambdalist &rest body)
  "Abbrev: (^ (x) body) <-> (lambda (x) body)"
  `(lambda ,lambdalist ,@body))

(defmacro another-name (alias name)
  (cond ((special-operator-p name) `(defmacro ,alias (&rest args)
                                     `(,',name ,@args)))
        ((macro-function name)     `(setf (macro-function ',alias)
                                          (macro-function ',name)))
        ((fboundp name)            `(setf (symbol-function ',alias)
                                          (function ,name)))
        (t (error "The name, ~A is not binding to a special-operator, macro, or function."
                  name))))

(eval-when (:compile-toplevel)
  (defun group (n lst)
    (if (zerop n) (error "zero length"))
    (labels ((rec (lst acc)
               (let ((rest (nthcdr n lst)))
                 (if (consp rest)
                     (rec rest (cons (subseq lst 0 n)
                                     acc))
                     (nreverse (cons lst acc))))))
      (if lst (rec lst nil) nil))))

(defmacro another-names (&rest names)
  `(progn ,@(mapcar (^ (pair) `(another-name ,@pair))
                    (group 2 names))))

(another-names  ~&  fresh-line
                1st first
                2nd second
                mvbind multiple-value-bind
                make-inst make-instance)

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar (^ (s) `(,s (gensym)))
                 syms)
     ,@body))

(defun file-to-string (file)
  (with-open-file (in file :direction :input)
    (let* ((f-len (file-length in))
           (str   (make-string f-len))
           (r-len (read-sequence str in)))
      (if (< r-len f-len)
          (subseq str 0 r-len)
          str))))

(defun str (&rest strings)
  (apply #'concatenate 'string strings))

(defun str->key (string)
  (intern (string-upcase string) :keyword))

(defmacro aif (test then &optional else)
  `(let ((it ,test))
     (if it ,then ,else)))

(defmacro awhen (test &body body)
  `(aif ,test
        (progn ,@body)))

(defun last1 (lst) (car (last lst)))

(defun conc1 (lst elt) (nconc lst (list elt)))

(defmacro unwind-delfile ((file) &body body)
  `(unwind-protect
       ,@body
     (when (probe-file ,file)
       (delete-file ,file))))

;;====================================================================