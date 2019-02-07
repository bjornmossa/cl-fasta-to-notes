(in-package :cl-user)

(defpackage #:fasta-notes.user
  (:use #:cl
        #:fasta-notes
        #:fasta-notes.input
        #:fasta-notes.output))

(in-package #:fasta-notes.user)

(defstruct (action
             (:print-function
              (lambda (struct stream depth)
                (declare (ignore depth))
                (format stream "~a to ~a"
                        (action-key struct)
                        (action-description struct)))))
  (key nil :type symbol)
  (description nil :type string))

(defparameter *actions*
  (map 'list
       #'(lambda (pair) (make-action :key (car pair)
                                :description (cdr pair)))
       '((Q . "quit")
         (O . "open a file")
         (C . "create a sequence")
         (S . "save a file"))))

(defun get-actions-exept (actions item)
  (remove-if #'(lambda (x) (eq item (action-key x))) actions))

(defun get-only-action (actions item)
  (remove-if-not #'(lambda (x) (eq item (action-key x))) actions))

(defun get-actions (current actions)
  (cond ((eq current 'Q) nil)
        ((eq current 'O) (get-only-action actions 'Q))
        ((or (eq current 'C) (eq current 'S)) (get-actions-exept actions current))))

(defun print-common-helper-text (current actions)
  (format nil "Press ~{~a~^, ~}." (get-actions current actions)))

(defmacro with-invite (body)
  `(progn
     (princ "Enter a command: ")
     ,body))

(defun repl ()
  (do ((command 'O))
      ((eq command 'Q) 'BYE)
    (with-invite
        (setf command (read)))))

;; Action structure
;;; -------limiter------
;;; - Common helper text
;;; -------limiter------
;;; - Action helper text
;;; - User input
