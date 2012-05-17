;;;; Last Updated : 2012/05/17 22:01:39 tkych

;; sesame in donuts/src/

;;====================================================================
;; Sesame
;;====================================================================
(in-package :in-donuts)

(defclass sesame ()
     ((tag :accessor :tag :initarg :tag)
      (treasure :accessor :treasure :initarg :treasure)))

(defun make-sesame (tag treasure)
  (make-inst 'sesame :tag tag :treasure treasure))

(defun sesame? (x) (typep x 'sesame))
(defun open-sesame (sesame) (:treasure sesame))

;;====================================================================