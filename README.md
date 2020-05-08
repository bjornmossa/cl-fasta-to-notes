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

At first you need to download a .fasta file with dna data. This package works with
files from https://www.ncbi.nlm.nih.gov/nuccore for shure. Please, create an issue
is you can not load a file.

For all user interruptions use `fasta-notes.user` package.
  ```common-lisp
  (in-package :fasta-notes.user)
  ```
For loading file use
  ```common-lisp
  (load-file)
  ```
And follow instructions.

For getting basic info about loaded file use
  ```common-lisp
  (file-info)
  ```
If you wand to check a sequence without saving use
  ```common-lisp
  (show-sequence)
  ```
### Export data
In `fasta-notes.user` package use
  ```common-lisp
  (save-sequence)
  ```
  
  `NB: During refactoring to faste-notes.user package LilyPond export NOT working. If you need it, please create an issue or PR using SuperCollider export as example.`
## License

GNU GPLv3

