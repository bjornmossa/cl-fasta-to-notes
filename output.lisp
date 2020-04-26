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

(defun get-degrees-strings ()
  '("c" "d" "e" "f" "g" "a" "b"))

(defun get-lillypond-note (codon-note)
  (if (codon-note-is-pause codon-note)
      (format nil "r~a"
              (codon-note-dur codon-note))
      (format nil "~a~a"
              (nth (codon-note-degree codon-note) (get-degrees-strings))
              (codon-note-dur codon-note))))

(defun list-to-string (lst)
  (reduce #'(lambda (acc val)
              (if (zerop (length acc))
                  val
                  (concatenate 'string acc " " val)))
          lst
          :initial-value ""))

(defun print-lillypond-code (stream notes)
  (format stream
           "\\relative c' {~C~C  ~a ~C~C}"
           #\return #\linefeed (list-to-string notes) #\return #\linefeed))

(defun get-lillypond-notes (models from to)
  (map 'list #'get-lillypond-note (subseq models from to)))

(defun make-lillypond-file (input from to &optional outpath)
  (with-open-file (out outpath
                       :direction :output
                       :if-exists :supersede)
    (print-lillypond-code out (get-lillypond-notes input from to))))
