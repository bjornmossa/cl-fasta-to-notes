;;;; fasta-notes.lisp

(defpackage #:fasta-notes
  (:use #:cl
        #:fasta-notes.model)
  (:export #:create-codon-models))

(in-package #:fasta-notes)

(defun nucleotide-list ()
  '(A G T C))

(defun nucleotide-values ()
  '(0 1 2 3))

(defun get-nucleotide-assocs ()
  (pairlis (nucleotide-list) (nucleotide-values)))

(defun get-nucleotide-value (nucleotide)
  (cdr (assoc (find-symbol (symbol-name nucleotide) 'fasta-notes) (get-nucleotide-assocs))))

(defun get-codon-values (codon)
  (map 'list #'get-nucleotide-value codon))

(defun codon-value (codon)
  (let ((val (get-codon-values codon)))
    (cons
     (reduce #'+ (butlast val))
     (reduce #'+ (rest val)))))

(defun get-dur (int)
  (nth int '(1 1/2 1/4 1/8 1/16 1/32 1/64 1/128)))

(defun zip-list (lst len &optional (result nil))
  (if (< (length lst) len)
      (reverse result)
      (zip-list
       (subseq lst len (length lst))
       len
       (cons (subseq lst 0 len) result))))

(defun stop-codon-p (codon)
  (not (every #'null
              (map 'list
                   #'(lambda (x) (equal codon x))
                   '((T G A) (T A G) (T A A))))))

(defun create-codon-model (codon)
  (let ((val (codon-value codon)))
    (if (stop-codon-p val)
        (make-codon-note :is-pause t
                         :dur (get-dur (cdr val)))
        (make-codon-note :degree (car val)
                         :dur (get-dur (cdr val))))))

(defun create-codon-models (codon-list start end)
  (map 'list #'create-codon-model (subseq (zip-list codon-list 3) start end)))
