(in-package :cl-user)

(defpackage #:fasta-notes.user
  (:use #:cl
        #:fasta-notes
        #:fasta-notes.input
        #:fasta-notes.output
        #:fasta-notes.model))

(in-package :fasta-notes.user)

(defparameter *file* '(ERROR . "No file selected"))

(defmacro with-file-env (file-env &rest body)
  (let ((file (gensym)))
    `(let ((,file ,file-env))
      (if (eq (car ,file) 'ERROR)
          (cdr ,file)
          ,@body))))

(defmacro with-path (var &body body)
  (let ((path (gensym)))
    `(progn
      (print "Enter path to save a file:")
      (let ((,path (read-line)))
        (if (null (directory ,path))
            (print "Directory doesn't exist")
            (let ((,var ,path))
              ,@body))))))

(defun get-safe-name (file-header-content)
  (let ((first-space (+ 1 (position #\Space file-header-content)))
        (header-length (length file-header-content)))
    (subseq file-header-content first-space header-length)))

(defun load-file ()
  (print "Enter path to .fasta file:")
  (let ((path (read-line)))
    (progn
      (setf *file* (read-fasta-file path))
      (with-file-env *file*
        (cdr *file*)))))

(defun file-info ()
  (with-file-env *file*
    (progn
      (print (fasta-file-header (cdr *file*)))
      (format t "~%File contains ~d nucleotides" (length (fasta-file-content (cdr *file*))))
      (format t "~%and ~d codons" (/ (length (fasta-file-content (cdr *file*))) 3)))))

(defun show-sequence (start end)
  (with-file-env *file*
    (let* ((codons (fasta-file-content (cdr *file*)))
           (sequence (create-codon-models codons start end))
           (durs (map 'list #'codon-note-dur sequence))
           (degrees (map 'list #'codon-note-degree sequence)))      
      (format t "Degrees: ~{~a~^, ~}~%Durations: ~{~a~^, ~}" degrees durs))))

(defun save-sc-file (start end)
  (with-path p
    (let* ((path (concatenate 'string p "/" (get-safe-name (fasta-file-header (cdr *file*))) ".scd"))
           (codons (fasta-file-content (cdr *file*))))
      (print "ok"))))


(defun save-sequence ()
  (with-file-env *file*
    (progn
      (format t "~%Choose format.~%1 - SuperCollider~%2 - LilyPond~%")
      (let ((choose (read-preserving-whitespace)))
      (cond
        ((eql choose 1)
         (format t "SC"))
        ((eql choose 2)
         (format t "LY"))
        (t
         (format t "Wrong option chosen")))))))
