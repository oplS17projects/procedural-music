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
         make-random-bpm)

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
;; range: 60 - 127, no insane low values

(define (make-random-velocity)
  (random 60 128))

;; make a random duration
;; in beats
;; range: specific durations

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

;; make a note given key when we want a random duration
(define (make-random-note-with-key key)
  (list (list-ref key (random (- (length key) 1))) (make-random-velocity) (make-random-duration)))

;; make a bar
;; takes a note as input, makes a bar of music based on the key
;;   time signature random between 4/4 or 3/4, i.e. 3 or 4 beats
;;   per bar
(define (make-random-bar initial-note beats)
  (define key (make-key (id-of initial-note)))
  (define (iter note index limit)
    (cond ((< (+ index (duration-of note)) limit)
           (cons note (iter (make-random-note-with-key key)
                                   (+ index (duration-of note))
                                   limit)))
          ((> (+ index (duration-of note)) limit)
           (cons (make-note-with-key key (- limit index))
                        '()))
          (else '())))

;  tail recursive proc
;  (define (iter bar note index limit)
;    (cond ((< (+ index (duration-of note)) limit)
;           (iter (stream-cons note bar)
;                 (make-random-note-with-key key)
;                 (+ index (duration-of note))
;                 limit))
;          ((> (+ index (duration-of note)) limit)
;           (stream-cons (make-note-with-key key (- limit index)) bar))
;           (else bar)))

  (iter initial-note 0 beats))

;; make a sequence
;; makes a bunch of random music bars based off given note, namely
;;   all based off the same key. procedure returns an infinite stream
;;   where it takes random bars from the list and repeats them 
(define (make-sequence note)
  (define beats (random 3 5))
  (define key (make-key (id-of note)))
  (define bars
    (build-list (random 1 10) (λ (x) (make-random-bar (make-random-note-with-key key) beats))))
  (define (iter bar)
    (cond ((equal? bar '()) (iter (list-ref bars (- (length bars) 1))))
          (else (stream-cons (stream-first bar) (iter (stream-rest bar))))))
  (iter (list-ref bars (- (length bars) 1))))

;; make note list
;; prettier version of the above

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
;;   used for make-sequence for sequences in particular key

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

;; make a bpm in range 60 to 120
(define (make-random-bpm) (random 60 120))

(make-note-list)