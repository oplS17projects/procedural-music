#lang racket

(require "generator.rkt")
(require rsound)
(require rsound/piano-tones)
(require portaudio
         ffi/vector)

;; math to change midi id into a frequency number
(define (midi->freq id)
  (* 27.5 (expt 2 (/ (- id 21) 12))))

;; makes a sine wave of the note, plays it, sleeps for the duration, stops after
(define (rs-play-note note bpm)
  (begin (signal-play (network () (lfo <= square-wave (midi->freq (id-of note)))))
  (sleep (bpm->second bpm (duration-of note)))
  (stop)))

;; iterates through the stream and plays each note until the stream is empty
(define (rs-play-stream stream)
  (let ((bpm (make-random-bpm)))
    (define (iter stream)
      (if (not (stream-empty? stream))
          (rs-play-note (stream-first stream) bpm)
          (sleep 0))
      (iter (stream-rest stream)))
    (iter stream)))

;; testing stuff
(rs-play-stream (make-note-list))