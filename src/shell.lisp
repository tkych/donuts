;;;; Last Updated : 2012/06/02 12:27:12 tkych

;; Shell interface for donuts

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
;; Shell of Donuts
;;====================================================================
(in-package :in-donuts)

(defparameter *viewer* #+windows                  "start"
                       #+(or darwin macos macosx) "open"
                       #+(or linux freebsd)       "xdg-open")

(defun make-dot-file (graph &optional
                      (file-name "DONUTS-DOT-TMP.dot")
                      (pprint? nil))
  (with-open-file (s file-name  :direction :output
                                :if-exists :supersede
                                :external-format :utf-8)
    (let ((*standard-output* s))
      (if pprint? (dot-pprint graph) (dot-output graph)))
    file-name))

(defun dot-pprint (graph)
  (let ((tmp-name (str "DONUTS-DOT-PPRINT-TMP-"
                       (format nil "~A" (get-universal-time))
                       ".dot")))
    (unwind-delfile (tmp-name)
      (output-pprint (make-dot-file graph tmp-name)))))

(defun output-pprint (dot-file)
  (format t "~A"
          (trivial-shell:shell-command
           (format nil "nop ~A" dot-file) :input "")))

(defun make-image (dot-file layout image-file)
  (trivial-shell:shell-command
   (format nil "dot -K~(~A~) -T~(~A~) ~A -o ~A"
           layout (pathname-type image-file) dot-file image-file)
   :input "")
  image-file)

(defun show-image (image-file)
  (trivial-shell:shell-command
   (format nil "~A ~A" *viewer* image-file) :input ""))

(defmacro $ ((&key (outfile "DONUTS-TMP.png" file?) (layout :dot)
                   (show t))
             graph)
  (with-gensyms (tmp-name)
    `(let ((,tmp-name ,(str "DONUTS-SHELL-TMP-"
                            (format nil "~A" (get-universal-time))
                            ".dot")))
       (unwind-delfile (,tmp-name)
         ,(if (string= "dot" (pathname-type outfile))
              `(progn
                 (make-dot-file ,graph ,outfile t)
                 (when (and ,show (truename ,outfile))
                   (format t "~A" (file-to-string ,outfile))))
              `(unwind-protect           
                   (make-image (make-dot-file ,graph ,tmp-name)
                               ,layout ,outfile)
                 (when (and ,show (probe-file ,outfile))
                   (show-image ,outfile))
                 (when (and (not ,file?) (probe-file "DONUTS-TMP.png"))
                   (delete-file "DONUTS-TMP.png")))))
       nil)))

;;====================================================================
