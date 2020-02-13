;;;; package.lisp

(defpackage #:fasta-notes
  (:use #:cl
        #:fasta-notes.model)
  (:export :get-nuc-assocs
           :get-stop-codons
           :get-nuc-assoc-val
           :get-assoc-numlist
           :find-nuc-assoc
           :is-nuc?
           :str-to-list
           :nuc-string?
           :is-stop-codon?
           :codon-to-assocs
           :codon-to-notes
           :split-string-by
           :create-model
           :zip-list))
