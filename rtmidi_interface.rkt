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


 
; Create RtMidiIn and RtMidiOut
(define in (make-rtmidi-in))
(define out (make-rtmidi-out))
 
; List input and output ports
(define in-ports (rtmidi-ports in))
(printf "Input ports: ~a~n" in-ports)
 
(define out-ports (rtmidi-ports out))
(printf "Output ports: ~a~n" out-ports)
 
; Open the first input and output port, if any
 
(match in-ports
  [(cons a-port other-ports) (rtmidi-open-port in 0)]
;;  [other (printf (current-error-port) "no input ports\n")])
  [other (printf "no input ports\n")])
 
(match out-ports
  [(cons a-port other-ports) (rtmidi-open-port out 0)]
;;  [other (printf (current-error-port) "no output ports\n")])
  [other (printf "no output ports\n")])

(define (send-midi-message status data1 data2) (rtmidi-send-message out (list status data1 data2)))

(define (play-midi-note length note on-veloc off-veloc channel) (thread (lambda () (begin (send-midi-message (+ channel 144) note on-veloc)
                                                                               (sleep length)
                                                                               (send-midi-message (+ channel 128) note off-veloc)
                                                                               (printf "note sent (play-midi-note)\n")))))

(sleep 15)

(play-midi-note 2 64 93 64 0)

(printf "note sent (main)\n")
 
(define (play-note note)
; Send a note
(rtmidi-send-message out (list (* 11 16) 7 128))
(printf "sending note~n")
(rtmidi-send-message out (list 144 65 96))
(sleep 2)
(rtmidi-send-message out (list 128 65 64))
(printf "note sent~n"))


;(sleep 10)

;(play-note)


(define (listen-midi-events)
; Read incoming messages until break
(let loop ()
  (pretty-print (sync in))
;  (play-note)
  (loop))
)

(define listenthread (thread listen-midi-events))

(sleep 60)

(kill-thread listenthread)