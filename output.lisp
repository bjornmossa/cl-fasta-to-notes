(in-package :cl-user)

(defpackage #:fasta-notes.output
  (:use #:cl
        #:fasta-notes
        #:fasta-notes.model))

(in-package #:fasta-notes.output)

(defun print-sc-code (amps durs degrees &optional (stream t))
  (format stream "Pbind(~C~C  \\amp, Pseq([~{~a~^, ~}], inf),~C~C  \\dur, Pseq([~{~a~^, ~}], inf),~C~C  \\degree, Pseq([~{~a~^, ~}], inf)~C~C);"
          #\return #\linefeed amps #\return #\linefeed durs #\return #\linefeed degrees #\return #\linefeed))

(defun sc-amp (codon-note)
  (if (codon-note-is-pause codon-note)
      0
      1))

(defun sc-dur (codon-note)
  (if (= 1 (codon-note-dur codon-note))
      (format nil "~A" (codon-note-dur codon-note))
      (format nil "1/~A" (codon-note-dur codon-note))))

(defun make-sc-amps (models)
  (map 'list #'sc-amp models))

(defun make-sc-durs (models)
  (map 'list #'sc-dur models))

(defun make-sc-file (input from to &optional outpath)
  (let* ((models (subseq input from to))
         (amps (make-sc-amps models))
         (durs (make-sc-durs models))
         (degrees (map 'list #'codon-note-degree models)))
    (with-open-file (out outpath
                         :direction :output
                         :if-exists :supersede)
      (print-sc-code amps durs degrees out))))
