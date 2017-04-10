#lang racket
(require rtmidi)
;; see the racket documentation for this package for additional install steps

;; the following are changes that need to be made to the Makefile for the rtmidi
;; package for racket.
;; system specific install commands are still needed for mac and windows
;; the following lines need to be added

;; +unix: CXXFLAGS += -D__UNIX_JACK__ -ljack
;; +unix: LDFLAGS += -ljack -lpthread
;; +unix: wrap-rtmidi.so

;; +install-unix: unix
;; +	cp wrap-rtmidi.so /usr/lib/

;; +install-linux: linux
;; +	cp wrap-rtmidi.so /usr/lib/



;; the following are changes that need to be made to the Makefile for RtMidi-2.1.0
;; the following line needs to be commented out

;; -	install --mode=644 RtMidi.h RtError.h $(PREFIX)/include


; to set up a port use the following call sequence

; (define in (make-in-port))
; (define out (make-out-port))
; (open-in-port in "MIDI-DEVICE")
; (open-out-port out "MIDI-DEVICE")

; replace "MIDI-DEVICE" with the appropriate midi device by calling the
; following procedures to find the name of the available devices
; the (open-in-port) and (open-out-port) procedures pick the first
; available port that has a name that contains "MIDI-DEVICE" as a substring

; (list-in-ports in)
; (list-out-ports out)

; the usb to midi cable I have is shown as

; "a2j:Turtle Beach USB MIDI 1x1 [20] (capture): Turtle Beach USB MIDI 1x1 MIDI "
; and
; "a2j:Turtle Beach USB MIDI 1x1 [20] (playback): Turtle Beach USB MIDI 1x1 MIDI "



; for mor information about the midi specifications, see www.midi.org/specifications
; refer to the General midi specification level 1



(provide make-in-port
         make-out-port
         list-in-ports
         list-out-ports
         open-in-port
         open-out-port
         close-in-port
         close-out-port
         send-midi-message
         read-midi-message
         note-off
         note-on
         play-note
         poly-key-pressure
         control-change
         program-change
         channel-pressure
         pitch-bend
         bank-select
         channel-volume-change
         set-pan
         set-expression-controller
         set-sustain
         set-sostenuto
         set-soft-pedal
         set-local-control)


; Create RtMidiIn and RtMidiOut
(define (make-in-port) (make-rtmidi-in))
(define (make-out-port) (make-rtmidi-out))

;list input and output ports
(define (list-in-ports in) (rtmidi-ports in))

(define (list-out-ports out) (rtmidi-ports out))


; Open the specified midi ports for input and output
(define (in-ports-length in) (length (list-in-ports in)))
(define (out-ports-length out) (length (list-out-ports out)))


(define (open-in-port in-port name)
  (define i 0)
  (for ([j (in-range (in-ports-length in-port))])
    #:break (string-contains? (list-ref (list-in-ports in-port) i) name)
    (set! i j))
  (if (not (= i (in-ports-length in-port)))
        (rtmidi-open-port in-port i)
        (printf "Port ~a not found" name)))


(define (open-out-port out-port name)
  (define i 0)
  (for ([j (in-range (out-ports-length out-port))])
    #:break (string-contains? (list-ref (list-out-ports out-port) i) name)
    (set! i j))
  (if (not (= i (out-ports-length out-port)))
      (rtmidi-open-port out-port i)
      (printf "Port ~a not found" name)))

; Close the in port
(define (close-in-port in-port) (rtmidi-close-port in-port))

; Close the out port
(define (close-out-port out-port) (rtmidi-close-port out-port))

; Sends a midi message to the port
(define (send-midi-message port status data1 data2)
  (rtmidi-send-message port (list status data1 data2)))

; Read a midi message from the port
(define (read-midi-message port)
  (sync port))



;; all values start from 0
;; channel 1 is channel number 0, channel 10 (purcusion) is channel number 9

; Procedures for sending midi messages

(define (note-off port channel note) (send-midi-message port (+ 128 channel) note 0))
(define (note-on port channel note velocity) (send-midi-message port (+ 144 channel) note velocity))

; plays a note for a specified length of time in seconds, length can have arbitrary precision
; 24 seems to be the lowest note
; 96 seems to be the highest note
(define (play-note port channel note velocity length)
  (thread (lambda () (note-on port channel note velocity)(sleep length)(note-off port channel note))))

; polyphonic key pressure (aftertouch)
(define (poly-key-pressure port channel note pressure) (send-midi-message port (+ 160 channel) note pressure))
(define (control-change port channel controller value) (send-midi-message port (+ 176 channel) controller value))
; program change is tone select
(define (program-change port channel program) (send-midi-message port (+ 192 channel) program 0))
; channel-pressure (aftertouch)
(define (channel-pressure port channel pressure) (send-midi-message port (+ 208 channel) pressure 0))
; pitch bend value is a 14 bit value, center (no change) is 8192 or lsb=0 msb=40
(define (pitch-bend port channel lsb msb) (send-midi-message port (+ 224 channel) lsb msb))


; control changes and mode changes
(define (bank-select port channel bank tone)
  (thread (lambda ()
            (send-midi-message port (+ 176 channel) 0 bank)
            (send-midi-message port (+ 176 channel) 32 0)
            (program-change port channel tone))))

(define (channel-volume-change port channel volume)
  (send-midi-message port (+ 176 channel) 7 volume))

;left-right balance
(define (set-pan port channel pan)
  (send-midi-message port (+ 176 channel) 10 pan))


; expression controller
(define (set-expression-controller port channel expression-value)
  (send-midi-message port (+ 176 channel) 11 expression-value))


; hold1 (sustain) pedal. set on to #t to enable sustain, set on argument to #f to turn off
(define (set-sustain port channel on)
  (send-midi-message port (+ 176 channel) 64 (if on 64 0)))

; Sostenuto pedal
(define (set-sostenuto port channel on)
  (send-midi-message port (+ 176 channel) 66 (if on 64 0)))

; Soft pedal
(define (set-soft-pedal port channel on)
  (send-midi-message port (+ 176 channel) 67 (if on 64 0)))

; local control on/off (should the keyboard make sounds when keys are pressed?)
; The piano we will be using for the demo doesn't recognize this message
(define (set-local-control port channel on)
  (send-midi-message port (+ 176 channel) 122 (if on 127 0)))





;;(define (play-midi-message-list lst) (begin (sleep (car lst)) (send-midi-message (cadr lst) (caddr lst) (cadddr lst)) (if (null? (cddddr lst)) 0 (play-midi-message-list (cddddr lst)))))

;;(define (parse-midi-message-list lst (
;; this will eventually be able to take a list of midi messages and create a list of midi notes and other midi messages


;(sleep 15)

;(play-midi-note 2 64 93 64 0)

;(printf "note sent (main)\n")
 
;(define (play-note note)
; Send a note
;(rtmidi-send-message out (list (* 11 16) 7 128))
;(printf "sending note~n")
;(rtmidi-send-message out (list 144 65 96))
;(sleep 2)
;(rtmidi-send-message out (list 128 65 64))
;(printf "note sent~n"))


;(sleep 10)

;(play-note)


;(define (listen-midi-events)
; Read incoming messages until break
;(let loop ()
;  (pretty-print (sync in))
;  (play-note)
;  (loop)))

;(define listenthread (thread listen-midi-events))

;(sleep 60)

;(kill-thread listenthread)

;(define (echo-midi-events)
;  (let loop ()
;    ((lambda (evnt) (pretty-print evnt)(sleep (car evnt))(send-midi-message (cadr evnt) (caddr evnt) (cadddr evnt))) (sync in))
;    (loop)))

;(define echothread (thread echo-midi-events))
