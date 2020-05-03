(in-package :cl-user)

(defpackage #:fasta-notes.utils
  (:use #:cl)
  (:export #:zip-list))

(in-package #:fasta-notes.utils)

(defun zip-list (lst len &optional (result nil))
  (if (< (length lst) len)
      (reverse result)
      (zip-list
       (subseq lst len (length lst))
       len
       (cons (subseq lst 0 len) result))))
