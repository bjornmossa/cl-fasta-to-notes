(in-package :cl-user)

(defpackage #:fasta-notes.model
  (:use #:cl)
  (:export :codon-note
           :make-codon-note
           :codon-note-dur
           :codon-note-degree
           :codon-note-is-pause))

(in-package #:fasta-notes.model)

(defstruct codon-note
  (dur 0 :type integer)
  (degree 0 :type integer)
  (is-pause nil :type boolean))
