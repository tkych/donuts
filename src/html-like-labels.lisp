;;;; Last Updated : 2012/05/10 20:57:59 tkych

;; Html-like-labels topping for donuts

;; Copyright (c) 2012 Takaya OCHIAI

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;====================================================================
;; Html-Like Labels
;;====================================================================
(in-package :in-donuts)

(defclass tag ()
     ((name  :accessor :name  :initarg :name  :initform nil)
      (pair? :accessor :pair? :initarg :pair? :initform t)
      (attrs :accessor :attrs :initarg :attrs :initform nil)
      (body  :accessor :body  :initarg :body  :initform nil)))

(defun tag? (x) (typep x 'tag))

(defun make-tag (name pair? &rest body)
  (mvbind (attrs contents) (scan-body body)
    (make-inst 'tag :name name :attrs attrs
                    :body contents :pair? pair?)))

(defun scan-body (body)
  (let ((toggle nil) (attrs nil) (contents nil))
    (dolist (elt body)
      (cond (toggle         (push elt attrs)
                            (setf toggle nil))
            ((keywordp elt) (push elt attrs)
                            (setf toggle t))
            (t              (push elt contents))))
    (values (nreverse attrs) (nreverse contents))))

(defun print-tag (tag)
  (with-slots (name body attrs pair?) tag
     (if pair?
         (progn
           (format t "~&    <~A~{ ~A=\"~(~A~)\"~}>" name attrs)
           (dolist (elt body)
             (if (tag? elt)
                 (print-tag elt)
                 (when elt (format t "~A" elt))))
           (format t "</~A>" name))
         (format t "<~A~{ ~A=\"~(~A~)\"~}/>" name attrs))))

(defmacro html (tag)
  (with-gensyms (s)
    `(cons :html-like-label
           (with-output-to-string (,s)
             (let ((*standard-output* ,s))
               (princ "<") (print-tag ,tag) (format t ">~&    "))))))

(defmacro def-tag (tag-name &optional (pair? t))
  (let ((body (gensym "attrs-tag-body-")))
    `(defun ,tag-name (&rest ,body)
       (apply #'make-tag ,(intern (symbol-name tag-name) :keyword)
                         ,pair? ,body))))

(defmacro def-tags (pair? names)
  `(progn ,@(mapcar (^ (name) `(def-tag ,name ,pair?)) names)))

(def-tags t (font i b u sub sup table tr td))
(def-tags nil (br hr vr img))

;;====================================================================