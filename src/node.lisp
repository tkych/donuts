;;;; Last Updated : 2012/05/21 18:36:24 tkych

;; node in donuts/src/

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
;; Node
;;====================================================================
(in-package :in-donuts)

(defparameter *with-node-context* nil)

(defmacro with-node ((&rest node-attrs) &body body)
  (let ((start (make-sesame :with-start
                            (format nil "~&  { node [~{~A=~A~^,~}];"
                                    (escape-attrs node-attrs))))
        (end (make-sesame :with-end (format nil "~&  };"))))
    `(let ((*with-node-context*
            (append *with-node-context* ',node-attrs)))
       (&& ',start ,@body ',end))))

(defun pre-node? (x) (or (stringp x) (numberp x)))

(defclass node ()
  ((name  :accessor :name  :initarg :name  :initform nil)
   (attrs :accessor :attrs :initarg :attrs :initform nil)
   (ports :accessor :ports :initarg :ports :initform nil)))

(defun node? (x) (typep x 'node))

(defmethod print-object ((obj node) stream)
  (print-unreadable-object (obj stream)
    (with-slots (name attrs) obj
       (format stream "~A :: ~{~A=~A~^ : ~}"
               name (escape-attrs attrs)))))

(defun name-of (node)
  (cond ((node? node)     (:name node))
        ((pre-node? node) node)
        (t                (error "~A is not node type." node))))

(defun <> (label &rest node-attrs)
  (make-inst 'node :name (format nil "node_~A" (gentemp "ID_"))
             :attrs (append `(:label ,label)
                            *with-node-context* node-attrs)))

;;--------------------------------------
(defclass record (node)
  ((ports :accessor :ports :initarg :ports :initform nil)))

(defun record? (x) (typep x 'record))

(defun escape-port (label)
  (ppcre:regex-replace-all
   "(^|{|\\|):(\\S+?)(?=\\b|}|\\|)" label "\\1<\\2>" :preserve-case t))

(defun find-port (label)
  (labels ((rec (start acc)
             (mvbind (match-start match-end regex-start regex-end)
                 (ppcre:scan "(^|{|\\|):(\\S+?)(?=\\b|}|\\|)"
                             label :start start)
               (if (null match-start)
                   acc
                   (rec match-end
                        (cons (str->key
                               (subseq label (aref regex-start 1)
                                       (aref regex-end 1)))
                              acc))))))
    (rec 0 nil)))

(defun [] (label &rest record-attrs)
  (make-inst 'record :name  (format nil "record_~A" (gentemp "ID_"))
                     :ports (find-port label)
                     :attrs (append `(:shape :record
                                      :label ,(escape-port label))
                                    *with-node-context* record-attrs)))

;;--------------------------------------
(defparameter *compass* '(:n :ne :e :se :s :sw :w :nw :c :_))
(defun compass-port? (x) (member x *compass*))
(defun port-exist? (node port) (member port (:ports node)))

;; hot spot?
(defun @ (node &rest ports)
  (dolist (port ports)
    (unless (keywordp port) (error "~A isn't keyword." port)))
  (let ((port-num (length ports)))
    (cond
      ((pre-node? node)
       (cond ((<= 2 port-num)
              (error "Too match port ~A." node))
             ((not (compass-port? (1st ports)))
              (error "Node ~S hasn't port :~(~A~)." node (1st ports)))
             (t (port-proc1 node ports))))
      ((record? node)
       (cond ((<= 3 port-num)
              (error "Too match port ~A." node))
             ((and (= 2 port-num)
                   (or (not (port-exist? node (1st ports)))
                       (not (compass-port? (2nd ports)))))
              (error "First port must be record-port,~@
                      and second port must be commpass-port."))
             ((and (= 1 port-num)
                   (not (port-exist? node (1st ports)))
                   (not (compass-port? (1st ports))))
              (error "Node ~S hasn't port :~(~A~)." node (1st ports)))
             (t (port-proc2 node ports))))
      ((node? node)
       (cond ((<= 3 port-num)
              (error "Too match port ~A." node))
             (t (port-proc2 node ports))))
      (t (error "~A isn't node type." node)))))

(defun port-proc1 (node ports)
  (make-sesame :port
               (format nil "~S:~(~A~)" node (1st ports))))

(defun port-proc2 (node ports)
  (make-inst 'node :attrs (:attrs node)
             :name (format nil "~A~{:~(~A~)~}" (:name node) ports)))

;;--------------------------------------
(defparameter *rank-pos* '(:same :min :max :source :sink))
(defun rank-pos? (x) (member x *rank-pos*))

(defun rank (pos &rest nodes)
  (if (not (member pos *rank-pos*))
      (error "~A is not a rank position." pos)
      (let ((rank-pos
             (format nil "~&  {rank=~(~A~);~{ ~A~^;~}};"
                     pos
                     (loop :for node :in nodes
                           :if (node? node) :collect (:name node)
                           :else :if (pre-node? node)
                           :collect (format nil "~S" node)))))
        (make-sesame :rank rank-pos))))

;;====================================================================