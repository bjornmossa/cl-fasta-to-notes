[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.txt)
![build](https://github.com/bjornmossa/cl-fasta-to-notes/workflows/build/badge.svg?branch=master)

# fasta-notes

Fasta-notes is a Common lisp tool for DNA data sonification
it provides several functions for converting [Fasta](https://en.wikipedia.org/wiki/FASTA) files
to SuperCollider patterns.

Export to other computer music enviroments as Pure Data or Max/MSP planned to the future.

## Installation

### project dependencies:

* [sbcl](http://www.sbcl.org)
* [quicklisp](https://www.quicklisp.org/beta/)

### Clone this repo

Clone this repo to you `quicklisp/local-projects` directory.
Ex: `git clone https://github.com/bjornmossa/cl-fasta-to-notes.git ~/quicklisp/local-projects`

### Build binary

With version 1.3.0 it is possible to make a binary. For building go to project directory and run make script
`cd ~/quicklisp/local-projects/cl-fasta-to-notes && make build`
After build process completed you can run programm in terminal:
`~/quicklisp/local-projects/cl-fasta-to-notes/build/fasta-notes`

If there is no permission set permissions for execution with
`chmod +x PATH_TO_BINARY`

## Using
### With binary
Run programm with:
`~/quicklisp/local-projects/cl-fasta-to-notes/build/fasta-notes`

#### Available commands
*load-file* - Read and process .fasta file
*file-info* - Show file basic information, organism name, number of codons and nucleotides
*show-sequence* - Show selected codons range as scale degrees and values
*save-sequence* - Save selected codons range as scale degrees and value to file

*help* - Print the help of all available commands.
*quit* - Quit the application.

You can use TAB for auto complete
### With SBCL
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

