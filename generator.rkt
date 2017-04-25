#lang racket

(require racket/stream)
;(require "rtmidi_interface.rkt")

(provide valid-notes
         make-random-id
         make-random-velocity
         make-random-duration
         make-random-note
         make-note
         make-note-with-key
         make-sequence
         make-note-list
         velocity-of
         duration-of
         id-of
         make-key
         bpm->second
         play-gnote
         play-sequence)

(define valid-notes (list 1/16 1/16 1/16 1/16 1/16 1/16 1/16 1/16
                          1/16 1/16 1/8 1/8 1/8 1/8 1/8 1/8 1/8 1/8
                          1/4 1/4 1/4 1/4 1/4 1/4 1/4 1/2 1/2 1/2 1
                          3/2 2 5/2 3 7/2 4))

;;;;;;; GENERATORS

;; make a random note id
;; default range: 36 - 95, i.e. no super insane low/hi notes
;; else range from low to high
(define make-random-id
  (lambda args
    (cond ((null? args) (random 36 96))
          ((null? (cdr args)) (random (car args)))
          ((null? (cddr args)) (random (car args) (cadr args)))
          (else (error "Invalid arguments for make-random-id")))))

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
    (list-ref valid-notes (random (- (length valid-notes) 1))))

;; make a random note
;; put id, velocity, duration in a list
(define make-random-note
  (lambda args
    (cond ((null? args) (list (make-random-id) (make-random-velocity) (make-random-duration)))
          ((null? (cdr args)) (list (make-random-id (car args)) (make-random-velocity) (make-random-duration)))
          ((null? (cddr args)) (list (make-random-id (car args) (cadr args)) (make-random-velocity) (make-random-duration)))
          (else (error "Invalid arguments for make-random-note")))))


;; make a specific note
(define (make-note note-id duration)
  (list note-id (make-random-velocity) duration))

;; make a specific note given key
(define (make-note-with-key key duration)
  (list (list-ref key (random (- (length key) 1))) (make-random-velocity) duration))

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

;;;;;; **will be fully done in milestone 3+**

(define (make-sequence note)

  (define valid-ids (make-key (id-of note)))
  (define new-note (make-note-with-key valid-ids (make-random-duration)))
  (stream-cons new-note (make-sequence note)))

;; make note list
;; stitches together notes into a list using recursion
;; basically appends sequences into a long list in a random number of times
;;   up to a limit
;; uses a list of sequences and pulls a random sequence to stich together
;;   from that list

;; MILESTONE 2: WIP. Converting to streams introduced so many problems that
;;   I didn't have time to get this working like it did before with lists.
;;   Implementation will be different with streams anyway, so I'm not
;;   upset about losing any progress.

(define (make-note-list)
  (make-sequence (make-random-note)))

;;;;;;; ACCESSORS

;; get the velocity from a note
(define (velocity-of note)
  (cadr note))

;; get duration of a note
(define (duration-of note)
  (caddr note))

;; get note id
(define (id-of note)
  (car note))

;;;;;; SPECIAL FUNCTIONS


;; make the key of a note
;; assume note is a 1, 4, or 5 and return key based on that
;; used for make-sequence for sequences in particular key
;; implementation: return a list of the notes than can be
;;    used for the key

(define (make-key note-id)
  (define (make-tonic-key note-id)
    (list note-id
          (+ note-id 2)
          (+ note-id 4)
          (+ note-id 5)
          (+ note-id 7)
          (+ note-id 9)
          (+ note-id 11)
          (+ note-id 12)))
  (define (make-subdom-key note-id)
    (list (- note-id 5)
          (- note-id 3) 
          (- note-id 1)
          note-id
          (+ note-id 2)
          (+ note-id 4)
          (+ note-id 6)
          (+ note-id 7))) 
  (define (make-dom-key note-id)
    (list (- note-id 7)
          (- note-id 5)
          (- note-id 3)
          (- note-id 2) 
          note-id
          (+ note-id 2)
          (+ note-id 4)
          (+ note-id 5)))
  (let ((tone-id (list-ref (list 1 4 5) (random 2))))
    (cond ((= tone-id 1) (make-tonic-key note-id))
          ((= tone-id 4) (make-subdom-key note-id))
          (else (make-dom-key note-id)))))

;; convert beat to seconds
(define (bpm->second bpm beat)
  (* (/ 60 bpm) beat))

;; play a note from generator through interface
(define (play-gnote port channel note bpm)
  1)

;; play a sequence through the interface
(define (play-sequence port channel stream bpm)
  1)