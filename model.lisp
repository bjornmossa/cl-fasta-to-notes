(in-package :cl-user)

(defpackage #:fasta-notes.model
  (:use #:cl)
  (:export :codon-note
           :make-codon-note
           :codon-note-dur
           :codon-note-degree
           :codon-note-is-pause))

(in-package #:fasta-notes.model)

(defun dur-p (dur)
  (or
   (typep dur 'integer)
   (typep dur 'ratio)))

(deftype dur ()
  `(satisfies dur-p))

(defstruct codon-note
  (dur 0 :type dur)
  (degree 0 :type integer)
  (is-pause nil :type boolean))
