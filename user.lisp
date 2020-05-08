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
       (finish-output)
       (let ((,path (read-line)))
         (if (null (directory ,path))
             (print "Directory doesn't exist")
             (let ((,var ,path))
               ,@body))))))

(defmacro with-selection (start end &body body)
  (let ((selection (gensym)))
    `(progn
       (print "Enter start and end values delimited by space:")
       (finish-output)
       (let ((,selection (with-input-from-string (st (read-line)) (list (read st) (read st)))))
         (let ((,start (car ,selection))
               (,end (car (cdr ,selection))))
           ,@body)))))

(defmacro with-safe-selection (file start end &body body)
  `(with-file-env ,file
     (with-selection ,start ,end
       ,@body)))

(defmacro with-sequence-selection (file var &body body)
  (let ((start (gensym))
        (end (gensym))
        (codons (gensym)))
    `(with-safe-selection ,file ,start ,end
       (let ((,codons (subseq (fasta-file-content (cdr ,file)) ,start ,end)))
         (let ((,var (create-codon-models ,codons)))
           ,@body)))))

(defmacro safe-sequence-export (file sequence path ext &body body)
  (let ((pth (gensym)))
    `(with-path ,pth
       (let ((,path (concatenate 'string ,pth "/" (get-safe-name (fasta-file-header (cdr ,file))) ,ext)))
         (with-sequence-selection ,file ,sequence
           ,@body)))))

(defun get-safe-name (file-header-content)
  (let ((first-space (+ 1 (position #\Space file-header-content)))
        (header-length (length file-header-content)))
    (subseq file-header-content first-space header-length)))

(defun load-file ()
  (print "Enter path to .fasta file:")
  (finish-output)
  (let ((path (read-line)))
    (progn
      (setf *file* (read-fasta-file path))
      (with-file-env *file*
        (cdr *file*)))))

(defun file-info ()
  (with-file-env *file*
    (progn
      (print (fasta-file-header (cdr *file*)))
      (format t "~%File contains ~d nucleotides:" (* 3 (length (fasta-file-content (cdr *file*)))))
      (format t "~%and ~d codons:" (length (fasta-file-content (cdr *file*)))))))

(defun show-sequence ()
  (with-sequence-selection *file* sequence
    (let ((durs (map 'list #'codon-note-dur sequence))
          (degrees (map 'list #'codon-note-degree sequence)))
      (format t "Degrees: ~{~a~^, ~}~%Durations: ~{~a~^, ~}" degrees durs))))

(defun save-sc-file ()
  (safe-sequence-export *file* seq path ".scd"
    (make-sc-file seq path)))

(defun save-sequence ()
  (with-file-env *file*
    (progn
      (format t "~%Choose format.~%1 - SuperCollider~%2 - LilyPond~%")
      (let ((choose (read)))
        (cond
          ((eql choose 1)
           (save-sc-file))
          ((eql choose 2)
           (format t "Lilypond export not implemented yet"))
          (t
           (format t "Wrong option chosen")))))))
