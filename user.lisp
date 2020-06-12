(in-package :cl-user)

(defpackage #:fasta-notes.user
  (:use #:cl
        #:fasta-notes
        #:fasta-notes.utils
        #:fasta-notes.input
        #:fasta-notes.output
        #:fasta-notes.model)
  (:export #:load-file
           #:file-info
           #:show-sequence
           #:save-sequence))

(in-package :fasta-notes.user)

(defstruct sequence-selection
  (start nil :type integer)
  (end nil :type integer)
  (data nil))

(defparameter *file* '(ERROR . "No file selected"))

(defmacro with-file-env (file-env &rest body)
  (let ((file (gensym)))
    `(let ((,file ,file-env))
      (if (eq (car ,file) 'ERROR)
          (format t "~a~%" (cdr ,file))
          ,@body))))

(defmacro with-path (var &body body)
  (let ((path (gensym)))
    `(progn       
       (format t "~%Enter path to save a file:~%")       
       (let ((,path (read-line)))
         (if (null (directory ,path))
             (format t "~%Directory doesn't exist~%")             
             (let ((,var ,path))
               ,@body))))))

(defmacro with-selection (start end &body body)
  (let ((selection (gensym)))
    `(progn
       (format t "~%Enter start and end values delimited by space:~%")       
       (let ((,selection (with-input-from-string (st (read-line)) (list (read st) (read st)))))
         (let ((,start (car ,selection))
               (,end (car (cdr ,selection))))
           (if (> ,start ,end)
               (format t "~%Selection start must be less then end~%")
               ,@body))))))

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
         (let ((,var (make-sequence-selection :start ,start
                                              :end ,end
                                              :data (create-codon-models ,codons))))
           ,@body)))))

(defmacro safe-sequence-export (file sequence path ext &body body)
  (let ((pth (gensym)))
    `(with-path ,pth
       (with-sequence-selection ,file ,sequence
         (let ((,path (concatenate 'string
                                   ,pth
                                   "/"
                                   (safe-organism-name (fasta-file-header (cdr ,file)))
                                   "_"
                                   (write-to-string (sequence-selection-start ,sequence))
                                   "_"
                                   (write-to-string  (sequence-selection-end ,sequence))
                                   ,ext)))
           ,@body)))))

(defun load-file ()
  "Read and process .fasta file"
  (format t "~%Enter path to .fasta file:~%")
  (let ((path (read-line)))
    (progn
      (setf *file* (read-fasta-file path))
      (with-file-env *file*
        (format t "~%File loaded~%")))))

(defun file-info ()
  "Show file basic information, organism name, number of codons and nucleotides"
  (with-file-env *file*
    (format t "~%~a~%File contains ~d nucleotides and ~d codons~%"
            (fasta-file-header (cdr *file*))
            (* 3 (length (fasta-file-content (cdr *file*))))
            (length (fasta-file-content (cdr *file*))))))

(defun show-sequence ()
  "Show selected codons range as scale degrees and values"
  (with-sequence-selection *file* sequence
    (let ((durs (map 'list #'codon-note-dur (sequence-selection-data sequence)))
          (degrees (map 'list #'codon-note-degree (sequence-selection-data sequence))))
      (format t "~%Degrees: ~{~a~^, ~}~%Durations: ~{~a~^, ~}~%" degrees durs))))

(defun save-sc-file ()
  (safe-sequence-export *file* seq path ".scd"
    (make-sc-file (sequence-selection-data seq) path)))

(defun save-sequence ()
  "Save selected codons range as scale degrees and valueto file"
  (with-file-env *file*
    (progn
      (format t "~%Choose format.~%1 - SuperCollider~%2 - LilyPond~%")
      (let ((choose (read)))
        (cond
          ((eql choose 1)
           (save-sc-file))
          ((eql choose 2)
           (format t "~%Lilypond export not implemented yet~%"))
          (t
           (format t "~%Wrong option chosen~%")))))))
