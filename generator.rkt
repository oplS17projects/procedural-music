#lang racket

;;;;;;; GENERATORS

;; make a random note id
;; range: 36 - 95, i.e. no super insane low/hi notes
(define (make-random-id)
  (random 36 96))

;; make a random velocity
;; range: 60 - 127
(define (make-random-velocity)
  (random 60 128))

;; make a random duration
;; in beats
;; range: specific durations
;; implementation: use list of ranges and recurse through with random int
;;   to pick one
;; 
(define (make-random-duration)
  (define valid-notes
    (list 1/16 1/8 1/4 1/2 3/4 1 1.5 2 2.5 3 3.5 4))
  (define (iter i end result)
    (if (< i end) (iter (+ i 1) end (cdr result))
        (car result)))
  (iter 0 (random (length valid-notes)) valid-notes))

;; make a random note
;; put id, velocity, duration in a list
(define (make-random-note)
  (list (make-random-id) (make-random-velocity) (make-random-duration)))

;; make a sequence
;; a short sequence of notes that sounds nice
;; this means that they need to:
;;;; - be within a similar octave range
;;;; - be in the same key
;;;; - be quantized to beats
;; Stretch goal: make this conform to a certain key.
;; Stretch goal: make velocity like natural performance (i.e. pseudorandom, and made
;;   to resemble a real person playing)
;; Stretch goal: make duration match other notes (i.e. sequences are consistent
;;   in being on-beat or syncopated. 1/16 or 1/8 note runs generally play in groups
;;   of 4 unless the beat is syncopated. basically make it sound much less random) 

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