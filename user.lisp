(in-package :cl-user)

(defpackage #:fasta-notes.user
  (:use #:cl
        #:fasta-notes
        #:fasta-notes.input
        #:fasta-notes.model))

(in-package :fasta-notes.user)

;; load file
;; show file info
(defparameter *file* '(ERROR . "No file selected"))

(defmacro with-file-env (file-env &rest body)
  (let ((file (gensym)))
    `(let ((,file ,file-env))
      (if (eq (car ,file) 'ERROR)
          (cdr ,file)
          ,@body))))

(let ((file '(ERROR . "No file selected")))

  (defun load-file ()
    (print "Enter path to .fasta file:")
    (let ((path (read-line)))
      (progn
        (setf file (read-fasta-file path))
        (if (eq (car file) 'ERROR)
            (cdr file)
            (fasta-file-header (cdr file))))))

  (defun file-info ()
    (if (eq (car file) 'ERROR)
        (cdr file)
        (progn
          (print (fasta-file-header (cdr file)))
          (format t "~%File contains ~d nucleotides" (length (fasta-file-content (cdr file))))
          (format t "~%and ~d codons" (/ (length (fasta-file-content (cdr file))) 3)))))

  (defun show-sequence (start end)
    (if (eq (car file) 'ERROR)
        (cdr file)
        (let* ((codons (fasta-file-content (cdr file)))
               (sequence (subseq (create-codon-models codons) start end))
               (durs (map 'list #'codon-note-dur sequence))
               (degrees (map 'list #'codon-note-degree sequence)))
          (format t "Degrees: ~{~a~^, ~}~%Durations: ~{~a~^, ~}" degrees durs))))

  )
