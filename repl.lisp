(defpackage #:fasta-notes.repl
  (:use #:cl))

(in-package :fasta-notes.repl)

(defun W ()
  (print "Hello from W"))

(defun test ()
  (let ((s (read)))
    (eval `(,s))))

;;; REPL
;; 1 - show avaible commands
;; 2 - read commad
;; 3 - eval command
;; 4 - print result

;; 1 - show avaible commands
;; commands - open file, show data, export, quit
;; if no file or error - open and quit
;; else all
(defstruct repl-env
  (file)
  (selection))

(defun is-valid-file (file)
  (not (or (null file) (eq (car file) 'ERROR))))

(defun get-usefull-commands (env)
  (let ((file (repl-env-file env))
        (commands '((O . "Open file")
                    (Q . "Quit")
                    (S . "Show data")
                    (E . "Export to file")))
        (no-file-commands '(O Q)))
    (if (is-valid-file file)
        commands
        (map 'list #'(lambda (x) (assoc x commands)) no-file-commands))))

(defun print-command (command)
  (format nil "~S: ~S" (car command) (cdr command)))
