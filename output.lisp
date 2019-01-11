(in-package :cl-user)

(defpackage #:fasta-notes.output
  (:use #:cl
        #:fasta-notes))

(in-package #:fasta-notes.output)

(defun print-sc-code (amps durs &optional (stream t))
  (format stream "pdef({~C~C  \\amp, Pseq([~{~a~^, ~}], inf),~C~C  \\dur, Pseq([~{~a~^, ~}], inf)~C~C});"
          #\return #\linefeed amps #\return #\linefeed durs #\return #\linefeed))
