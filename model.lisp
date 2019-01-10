(in-package :cl-user)

(defpackage #:fasta-notes.model
  (:use #:cl)
  (:export :codon-note))

(in-package #:fasta-notes.model)

(defstruct codon-note
  (octave 0 :type integer)
  (degree 0 :type integer)
  (is-pause nil :type boolean))
