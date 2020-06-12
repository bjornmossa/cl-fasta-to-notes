(in-package :cl-user)

(defpackage #:fasta-notes.readline
  (:use #:cl))

(in-package :fasta-notes.readline)

(replic.completion:functions-to-commands :replic.base)
(replic.completion:functions-to-commands :fasta-notes.user)
