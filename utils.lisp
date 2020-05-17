(in-package :cl-user)

(defpackage #:fasta-notes.utils
  (:use #:cl
        #:cl-ppcre)
  (:export #:zip-list
           #:safe-organism-name))

(in-package #:fasta-notes.utils)

(defun zip-list (lst len &optional (result nil))
  (if (< (length lst) len)
      (reverse result)
      (zip-list
       (subseq lst len (length lst))
       len
       (cons (subseq lst 0 len) result))))

(defun words-only (string)
  (format nil "~{~A~^~}" (all-matches-as-strings "\\w+" string)))

(defun get-organism-names (fasta-header)
  (subseq (cdr (split "\\s+" fasta-header)) 0 2))

(defun safe-organism-name (fasta-header)
  (format nil "~{~(~a~)~^_~}" (map 'list #'words-only (get-organism-names fasta-header))))
