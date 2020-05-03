(in-package :cl-user)

(defpackage #:fasta-notes.input
  (:use #:cl
        #:fasta-notes.utils)
  (:export :fasta-file
           :fasta-file-header
           :fasta-file-content
           :read-fasta-file))

(in-package #:fasta-notes.input)

(defstruct fasta-file
  (header "" :type string)
  (content nil :type list))

(defun open-file (path)
  (let ((stream (open path :if-does-not-exist nil)))
    (if (null stream)
        '(ERROR . "file doesn't exist")
        `(OK . ,stream))))

(defun newlinep (char)
  (char= char #\Newline))

(defun char-to-symbol (char)
  (if (newlinep char)
      nil
      (make-symbol (string char))))

(defun read-fasta-body (stream &optional (result nil))
  (let ((next-char (read-char stream nil)))
    (if (null next-char)
        (zip-list (remove-if #'null result) 3) 
        (read-fasta-body stream (append result (list (char-to-symbol next-char)))))))

(defun read-fasta-file (path)
  "Return list of symbol and reading result. fasta-file if result is OK or error string if ERROR"
  (let ((stream (open-file path))
        (file (make-fasta-file)))
    (if (eq 'ERROR (car stream))
        stream
        (progn
          (setf (fasta-file-header file) (read-line (cdr stream)))
          (setf (fasta-file-content file) (read-fasta-body (cdr stream)))
          (close (cdr stream))
          `(OK . ,file)))))
