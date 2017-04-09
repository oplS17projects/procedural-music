#lang racket

;;;;;;; GENERATORS

;; make a random note id
;; range: 36 - 95

;; make a random velocity

;; make a random duration

;; make a random note

;; make a sequence
;; a short sequence of notes that sounds nice
;; this means that they need to:
;;;; - be within a similar octave range
;;;; - be in the same key
;;;; - be quantized to beats
;; Stretch goal: make this conform to a certain key.
;; Stretch goal: make velocity like natural performance (i.e. pseudorandom)

;; possible implementation
;; (make-sequence (make-random-note))

;;;;;; **will be done in milestone 3+**
;;;;;; currently make-sequence is just an alias for make-note

;; make note list
;; stitches together notes into a list using recursion
;; basically appends sequences into a long list in a random number of times
;;   up to a limit
;; uses a list of sequences and pulls a random sequence to stich together
;;   from that list

;;;;;;; ACCESSORS

;; get the velocity from a note

;; get duration of a note

;; get note id

;;;;;; SPECIAL FUNCTIONS

;; get the key of a note
;; assume note is a 1, 4, or 5 and return key based on that
;; used for make-sequence for sequences in particular key
;;;;; possibly milestone 3+

;; convert note to midi message
;; takes the note id/velocity/duration list
;; makes note on/note off messages with a sleep in between
;; direct communication procedure with interface
;;;;;; **will be done in milestone 2+**