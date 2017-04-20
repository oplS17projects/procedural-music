#lang racket
(require rtmidi)
(require midi-readwrite)
(require control)
;; see the racket documentation for the rtmidi package for additional install steps

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



;; until the racket library midi-readwrite is updated with my fix, the following file needs to be changed.
;;
;; /home/USERNAME/.racket/6.8/pkgs/midi-readwrite/midi-readwrite/midi-read.rkt
;;
;; the following changes need to be made
;;
;; (define channel (bitwise-and #x7 next-byte))
;; needs to be changed to
;; (define channel (bitwise-and #xf next-byte))
;;
;; and
;;
;; (define channel (bitwise-and #x7 prior-event-type-byte))
;; needs to be changed to
;; (define channel (bitwise-and #xf prior-event-type-byte))
;;
;; this is to fix an issue where 




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

; for info on setting up midi devices and software on linux, refer to
; http://tedfelix.com/linux/linux-midi.html

; for info on setting up midi devices and software on windows, refer to
; http://donyaquick.com/midi-on-windows/

; makes the following procedures available to code that require this file
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
         set-local-control
         play-midi-file
         play-midi-data
         play-midi-track
         play-midi-stream
         ; Still need to implement the following:
         record-midi-in-to-file
         record-midi-out-to-file
         echo-midi-out-to-console
         echo-midi-in-to-console
         echo-midi-in-to-out)
         ;
         ;others?




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
(define (channel-all-notes-off port channel)
  (send-midi-message port (+ 176 channel) 123 0))

(define (channel-reset-all-controllers port channel)
  (send-midi-message port (+ 176 channel) 121 0))

(define (device-all-notes-off port)
  (for ([i (in-range 0 15)])
    (channel-all-notes-off port i)))

(define (device-reset-all-controllers port)
  (for ([i (in-range 0 15)])
    (channel-reset-all-controllers port i)))

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
  (send-midi-message port (+ 176 channel) 64 (cond ((eq? on #f) 0)
                                                   ((eq? on #t) 64)
                                                   (else on))))
                                             ;(if on 64 0)))

; Sostenuto pedal
(define (set-sostenuto port channel on)
  (send-midi-message port (+ 176 channel) 66 (cond ((eq? on #f) 0)
                                                   ((eq? on #t) 64)
                                                   (else on))))
                                             ;(if on 64 0)))

; Soft pedal
(define (set-soft-pedal port channel on)
  (send-midi-message port (+ 176 channel) 67 (cond ((eq? on #f) 0)
                                                   ((eq? on #t) 64)
                                                   (else on))))
                                             ;(if on 64 0)))

; local control on/off (should the keyboard make sounds when keys are pressed?)
; The piano we will be using for the demo doesn't recognize this message
(define (set-local-control port channel on)
  (send-midi-message port (+ 176 channel) 122 (cond ((eq? on #f) 0)
                                                   ((eq? on #t) 127)
                                                   (else on))))
                                             ;(if on 127 0)))


; plays a midi track with the given tempo
; tempo is microseconds per quarter beat, or bpm * 60,000,000.
; PPQN is pulses per quarter note, or the resolution of the midi-track (also called ticks per quarter note)
; track is a midi track structure from midi-readwrite
; port is an out-port
; channel is now taken from the midi messages instead of being passed to this procedure
(define (play-midi-track BPM PPQN track port); channel)
  (thread (λ ()
            (define midi-channel -1)
            (let ([time 0]
                  [secondsPerTick (/ 60 (* BPM PPQN))])
              (while (not (null? track))
                     ;(thread (λ () (println time)))
                     (if (= time (caar track))
                         (begin
                           (cond ((null? track) 0)
                                 ((ChannelMessage? (cadar track))
                                  (set! midi-channel (ChannelMessage-channel (cadar track)))
                                  (cond ((equal? (ChannelMessage-kind (cadar track)) 'note-off)
                                         (note-off port (ChannelMessage-channel (cadar track)) (car (ChannelMessage-operands (cadar track)))))
                                        ((equal? (ChannelMessage-kind (cadar track)) 'note-on)
                                         (note-on port (ChannelMessage-channel (cadar track)) (car (ChannelMessage-operands (cadar track))) (cadr (ChannelMessage-operands (cadar track)))))
                                        ((equal? (ChannelMessage-kind (cadar track)) 'aftertouch)
                                         (poly-key-pressure port (ChannelMessage-channel (cadar track)) (car (ChannelMessage-operands (cadar track))) (cadr (ChannelMessage-operands (cadar track)))))
                                        ((equal? (ChannelMessage-kind (cadar track)) 'control-change)
                                         (control-change port (ChannelMessage-channel (cadar track)) (car (ChannelMessage-operands (cadar track))) (cadr (ChannelMessage-operands (cadar track)))))
                                        ((equal? (ChannelMessage-kind (cadar track)) 'program-change)
                                         (program-change port (ChannelMessage-channel (cadar track)) (car (ChannelMessage-operands (cadar track)))))
                                        ((equal? (ChannelMessage-kind (cadar track)) 'channel-aftertouch)
                                         (channel-pressure port (ChannelMessage-channel (cadar track)) (car (ChannelMessage-operands (cadar track)))))
                                        ((equal? (ChannelMessage-kind (cadar track)) 'pitch-bend)
                                         (pitch-bend port (ChannelMessage-channel (cadar track)) (car (ChannelMessage-operands (cadar track))) (cadr (ChannelMessage-operands (cadar track)))))))
                                 ; System Exclusive messages need to be handled
                                 ((SysexMessage? (cadar track))
                                  0)
                                 ; Midi File Meta messages need to be handled
                                 ((MetaMessage? (cadar track)) 
                                  (cond ((equal? 'set-tempo (MetaMessage-content (cadar track)))
                                         (set! BPM (/ 60000000 (cadr (MetaMessage-content (cadar track)))))))))
                           ;(play-midi-track tempo (cdr track) port channel)
                           (set! track (cdr track))
                           (sleep 0))
                         (begin
                           (set! time (+ time 1))
                           (sleep secondsPerTick))))))))


(define (get-tempo-from-meta-track track)
  (define tempo 0)
  (while (= tempo 0)
         (begin
;           (pretty-print (cadar track))
           (cond ((MetaMessage? (cadar track))
                  (if (equal? 'set-tempo (car (MetaMessage-content (cadar track))))
                      (set! tempo (cadr (MetaMessage-content (cadar track))))
                      0))
                 ((SysexMessage? (cadar track))
                  0)
                 ((ChannelMessage? (cadar track))
                  0))
           (set! track (cdr track))))
;  (pretty-print tempo)
  tempo)
                    

; path is a the file path to a standard midi formated midi file, *.mid
(define (play-midi-file path out-port)
  (play-midi-data (midi-file-parse path) out-port))

; midi-data is a midi-file structure from midi-readwrite
; port should be an out port
(define (play-midi-data midi-data out-port)
;  (thread (λ ()
  (device-all-notes-off out-port)
  (device-reset-all-controllers out-port)
  (define midi-worker-threads '())
            (let* ([format (MIDIFile-format midi-data)]
                   [division (MIDIFile-division midi-data)]
                   [tracks (MIDIFile-tracks midi-data)]
                   [BPM (/ 60000000 (get-tempo-from-meta-track (list-ref tracks 0)))])
              (for ([i (in-range 0 (length tracks))])
                ;(pretty-print (if (> i 0) (ChannelMessage-channel (cadadr (list-ref tracks i))) -1))
                (set! midi-worker-threads (append midi-worker-threads (list (play-midi-track BPM (TicksPerQuarter-ticks division) (list-ref tracks i) out-port))))))
  midi-worker-threads)



; midi-stream is a stream that gives a sequence of lists that contain a delay from the previous event, measured in seconds,
; and a list
; (list 0.5 (list 'event-type channel data-byte-1 data-byte-2))
; where 
(define (play-midi-stream out-port midi-stream)
  (while (not (stream-empty? midi-stream))
         (let ([midi stream-first])
           (begin
             (cond ((null? midi) 0)
                   ((equal? (list-ref (cadr midi) 1) 'note-off)
                    (sleep (car midi))
                    (note-off out-port (list-ref (cadr midi) 2) (list-ref (cadr midi) 3)))
                   ((equal? (list-ref (cadr midi) 1) 'note-on)
                    (sleep (car midi))
                    (note-on out-port (list-ref (cadr midi) 2) (list-ref (cadr midi) 3) (list-ref (cadr midi) 4)))
                   ((equal? (list-ref (cadr midi) 1) 'aftertouch)
                    (sleep (car midi))
                    (poly-key-pressure out-port (list-ref (cadr midi) 2) (list-ref (cadr midi) 3) (list-ref (cadr midi) 4)))
                   ((equal? (list-ref (cadr midi) 1) 'control-change)
                    (sleep (car midi))
                    (control-change out-port (list-ref (cadr midi) 2) (list-ref (cadr midi) 3) (list-ref (cadr midi) 4)))
                   ((equal? (list-ref (cadr midi) 1) 'program-change)
                    (sleep (car midi))
                    (program-change out-port (list-ref (cadr midi) 2) (list-ref (cadr midi) 3) (list-ref (cadr midi) 4)))
                   ((equal? (list-ref (cadr midi) 1) 'channel-aftertouch)
                    (sleep (car midi))
                    (channel-pressure out-port (list-ref (cadr midi) 2) (list-ref (cadr midi) 3) (list-ref (cadr midi) 4)))
                   ((equal? (list-ref (cadr midi) 1) 'pitch-bend)
                    (sleep (car midi))
                    (pitch-bend out-port (list-ref (cadr midi) 2) (list-ref (cadr midi) 3) (list-ref (cadr midi) 4)))
                   ; System Exclusive messages need to be handled
                   ; Midi File Meta messages need to be handled
                   (else 0))
             (play-midi-stream out-port (stream-rest midi-stream))))))



;(define in (make-in-port))
;(define out (make-out-port))
;(open-in-port in "keyboard")
;(open-out-port out "FLUID")
;(open-in-port in "RtMidi")
;(define listenthread
;  (thread
;   (λ ()
;     (let loop ()
;       (pretty-print (sync in))
;       (loop)))))
;(define midi-threads (play-midi-file "/home/samuel/Midi_files/Wishmaster.mid" out))
;(pretty-print midi-threads)
;(define (wait-for-threads lst-threads)
;  (if (null? lst-threads)
;      'done
;      (begin
;        (thread-wait (car lst-threads))
;        (wait-for-threads (cdr lst-threads)))))
;(wait-for-threads midi-threads)
;(device-all-notes-off out)
;(device-reset-all-controllers out)
  
; how to play midi files

;(define in (make-in-port))
;(define out (make-out-port))
;(open-out-port out "FLUID")
;(open-in-port in "keyboard")
;(open-in-port in "RtMidi")
;(define (listen-midi-events)
;  (let loop ()
;    (pretty-print (sync in))
;    (loop)))
;(define listenthread (thread listen-midi-events))
;5
;(sleep 1)
;4
;(sleep 1)
;3
;(sleep 1)
;2
;(sleep 1)
;1
;(sleep 1)
;"Playing"
;(define pinball-thread (play-midi-file "/home/samuel/Midi_files/PINBALL.MID" out))
;(sleep 540)
;(define gm-test-thread (play-midi-file "/home/samuel/Midi_files/GM_Test.mid" out))


; Placeholder implementation, call sequences subject to change

(define (record-midi-in-to-file port-in file-path)
  0) ;TODO

(define (record-midi-out-to-file port-out file-path)
  0) ;TODO

(define (echo-midi-out-to-console port-out)
  0) ;TODO

(define (echo-midi-in-to-console port-in in-device-name)
  0) ;TODO

(define (echo-midi-in-to-out in-port out-port in-device-name out-device-name)
  0) ;TODO


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
