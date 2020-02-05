(in-package :cl-user)

(defpackage #:fasta-notes.input
  (:use #:cl
        #:fasta-notes))

(in-package #:fasta-notes.input)

;; (defun read-file (path lines)
;;   (let ((result ""))
;;     (progn
;;       (with-open-file (file path :direction :input)
;;         (do ((line (read-line file nil 'eof)
;;                    (read-line file nil 'eof))
;;              (i 0 (if (nuc-string? line) (+ i 1) i)))
;;             ((> i lines)
;;              (eql line 'eof))
;;           (if (nuc-string? line)
;;               (setf result (concatenate 'string result line)))))
;;       result)))

;; (defun file-to-models (file &optional (lines 10))
;;   (let* ((fasta-string (read-file file lines))
;;          (codon-list (split-string-by fasta-string 3)))
;;     (map 'list #'create-model codon-list)))

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
      (intern (string char))))

(defun read-fasta-body (stream &optional (result nil))
  (let ((next-char (read-char stream nil)))
    (if (null next-char)
        (remove-if #'null result)
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
