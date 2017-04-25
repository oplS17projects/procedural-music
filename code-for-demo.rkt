#lang racket

(require (file "Procedural-Music/procedural-music/rtmidi_interface.rkt"))
(require (file "Procedural-Music/procedural-music/generator.rkt"))


(define (process-stream channel notestream bpm)
  (stream-cons
   (list
    (* (/ bpm 60) (make-random-duration))
    (list
     'note-on
     channel
     (id-of (stream-first notestream))
     (velocity-of (stream-first notestream))))
   (stream-cons
    (list
     (* (/ bpm 60) (duration-of (stream-first notestream)))
     (list
      'note-off
      channel
      (id-of (stream-first notestream))
      0))
    (process-stream channel (stream-rest notestream) bpm))))



(define out (make-out-port))
(open-out-port out "FLUID")
(open-out-port out "monitor")
(define test-stream-1 (process-stream 0 (make-note-list) 120))
;(define test-stream-2 (process-stream 9 (make-note-list) 120))
;(define test-stream-3 (process-stream 11 (make-note-list) 120))
;(define test-stream-4 (process-stream 14 (make-note-list) 120))
(define t1 (play-midi-stream out test-stream-1))
;(define t2 (play-midi-stream out test-stream-2))
;(define t3 (play-midi-stream out test-stream-3))
;(define t4 (play-midi-stream out test-stream-4))



(define (generate-chord-secondary notestream)
  (stream-cons
   (make-key (id-of (stream-first notestream)))
   (generate-chord-secondary (stream-rest notestream))))