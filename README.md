# fasta-notes

Fasta-notes is a Common lisp tool for DNA data sonification
it provides several functions for converting [Fasta](https://en.wikipedia.org/wiki/FASTA) files
to SuperCollider patterns.

Export to other computer music enviroments as Pure Data or Max/MSP planned to the future.

## Using

### project dependencies:

* [sbcl](http://www.sbcl.org)
* [quicklisp](https://www.quicklisp.org/beta/)

### Clone this repo

Clone this repo to you `quicklisp/local-projects` directory.
Ex: `git clone https://github.com/Arseniusz/cl-fasta-to-notes.git ~/quicklisp/local-projects`

### Load it via quicklisp

With sbcl repl run `(ql:quickload :fasta-notes)`

### Saving DNA data

You need to to into `fasta-notes.input` package and save a .fasta file you work with:

  ```common-lisp
  (in-package :fasta-notes.input)
  (defparameter *mydata* "path-to-file.fasta" optional-count-of-lines-to-process)
  ```

Now you can access data with `fasta-notes.input::*mydata*` variable.

### Export data

  ```common-lisp
  (in-package :fasta-notes.output)
  (make-sc-file fasta-notes.input::*mydata* from to "path-to-output-file.scd")
  ```
'from' and 'to' is integers represent codons needs to process.

  ```common-lisp
  (make-sc-file fasta-notes.input::*mydata* 0 4 "path-to-output-file.scd")
  ```

will create a pattern with length of 4 starts at first codon and ends on forth.

## Testing
  For running unit tests load both `:fasta-notes` and `:fasta-notes-test` systems and run `:prove` test
  ```common-lisp
  (ql:quickload :fasta-notes)
  (ql:quickload :fasta-notes-test)
  (prove:run :fasta-notes-test)
  ```

## License

GNU GPLv3

