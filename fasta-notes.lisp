;;;; fasta-notes.lisp

(in-package #:fasta-notes)

(defun get-nuc-assocs ()
  "Return list of cons pairs where car is nucleotide char and cdr is int"
  '((#\A . 0) (#\G . 1) (#\T . 2) (#\C . 3)))

(defun get-stop-codons ()
  "Return list of stop codons as Strings"
  '("TGA" "TAG" "TAA"))

(defun get-nuc-assoc-val (nuc assoc)
  "Return Integer if nucleotide is in cons pair or nil"
  (if (char= nuc (car assoc))
      (cdr assoc)
      nil))

(defun get-assoc-numlist (nuc)
  "Return list of possible associations for nucleotide or nils"
  (map
   'list
   #'(lambda (x) (get-nuc-assoc-val nuc x))
   (get-nuc-assocs)))

(defun find-nuc-assoc (nuc)
  "Return Integer, associated with nucleotide or nil"
  (find-if-not #'null (get-assoc-numlist nuc)))

(defun is-nuc? (n)
  "Nucleotide predicate"
  (not (null (find-nuc-assoc n))))

(defun str-to-list (str)
  "Turn string into list of chars"
  (coerce str 'list))

(defun nuc-string? (str)
  (every #'is-nuc? (str-to-list str)))

(defun is-stop-codon? (str)
  "Stop codon predicate"
  (member str (get-stop-codons) :test #'string-equal))

(defun codon-to-assocs (str)
  (map 'list #'find-nuc-assoc (str-to-list str)))

(defun codon-to-notes (str)
  "Return cons pair represent a note and dur"
  (let ((assocs (codon-to-assocs str)))
    (cons
     (car assocs)
     (+ (second assocs) (third assocs)))))

(defun split-string-by (string n &optional result)
  (if (< (length string) n)
      result
      (split-string-by
       (subseq string n)
       n
       (append result (list (subseq string 0 n))))))

(defun get-dur (int)
  (nth int '(1 2 4 8 16 32 64 128)))

(defun create-model (string)
  (if (is-stop-codon? string)
      (make-codon-note :is-pause t
                       :dur (get-dur (car (codon-to-notes string))))
      (make-codon-note :dur (get-dur (car (codon-to-notes string)))
                       :degree (cdr (codon-to-notes string)))))


;; NEW STUFF
(defun nucleotide-list ()
  '(A G T C))

(defun nucleotide-values ()
  '(0 1 2 3))

(defun get-nucleotide-assocs ()
  (pairlis (nucleotide-list) (nucleotide-values)))

(defun get-nucleotide-value (nucleotide)
  (cdr (assoc nucleotide (get-nucleotide-assocs))))

(defun get-codon-values (codon)
  (map 'list #'get-nucleotide-value codon))

(defun codon-value (codon)
  (cons
   (reduce #'+ (butlast codon))
   (reduce #'+ (rest codon))))

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
  (let ((value (codon-value codon)))
    (if (stop-codon-p value)
        (make-codon-note :is-pause t
                         :dur (get-dur (cdr value)))
        (make-codon-note :degree (car value)
                         :dur (get-dur (cdr value))))))
