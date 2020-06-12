.PHONY: build
LISP?=sbcl

build:
	$(LISP)	--non-interactive --eval "(asdf:operate :build-op :fasta-notes)"
