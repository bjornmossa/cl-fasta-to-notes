.PHONY: build
LISP?=sbcl

build:
	$(LISP)	--non-interactive \
          --load fasta-notes.asd \
          --eval '(ql:quickload :fasta-notes)' \
          --eval '(asdf:make :fasta-notes)'
