# Procedural-Music

## Samuel Toups
### May 6, 2017

# Overview
This code allows for communication between racket code and midi devices. Procedures are provided for creating and accessing virtual midi ports, which can be connected to midi software and hardware.

Also provided are procedures for playing streams and sequences of midi events, as well as easily sending some common midi events.

**Authorship note:** All of the code described here was written by myself.

# Libraries Used
The code uses two main libraries:

```
(require rtmidi)
(require midi-readwrite)
```

* The ```rtmidi``` library provides a racket interface to the RtMidi c++ library, which provides virtual midi ports and methods for reading and writing to them
* The ```midi-readwrite``` library is used to access and manipulate midi data

# Key Code Excerpts

Here is a discussion of the most essential procedures, with a focus on **how they embody ideas from 
UMass Lowell's COMP.3010 Organization of Programming languages course.** 

Four examples are shown and they are individually numbered. The titles of each section highlight the course concepts embodied.

## 1. Using Streams to play Midi data

The following code iterates through a stream of midi data and sends the appropriate midi event to a midi port.

```
(define (play-midi-stream out-port midi-stream)
  (thread (λ () (play-midi-stream-iter out-port midi-stream))))

(define (play-midi-stream-iter out-port midi-stream)
  (if (not (stream-empty? midi-stream))
      (let ([midi (stream-first midi-stream)])
          ;(pretty-print (stream-ref midi-stream 0))
          (cond ((null? midi) 0)
                ((equal? (type-of midi) 'note-off)
                 (sleep (delay-of midi))
                 (note-off out-port (channel-of midi) (first-data-byte-of midi)))
                ((equal? (type-of midi) 'note-on)
                 (sleep (delay-of midi))
                 (note-on out-port (channel-of midi) (first-data-byte-of midi) (second-data-byte-of midi)))
                ((equal? (type-of midi) 'aftertouch)
                 (sleep (delay-of midi))
                 (poly-key-pressure out-port (channel-of midi) (first-data-byte-of midi) (second-data-byte-of midi)))
                ((equal? (type-of midi) 'control-change)
                 (sleep (delay-of midi))
                 (control-change out-port (channel-of midi) (first-data-byte-of midi) (second-data-byte-of midi)))
                ((equal? (type-of midi) 'program-change)
                 (sleep (delay-of midi))
                 (program-change out-port (channel-of midi) (first-data-byte-of midi) (second-data-byte-of midi)))
                ((equal? (type-of midi) 'channel-aftertouch)
                 (sleep (delay-of midi))
                 (channel-pressure out-port (channel-of midi) (first-data-byte-of midi) (second-data-byte-of midi)))
                ((equal? (type-of midi) 'pitch-bend)
                 (sleep (delay-of midi))
                 (pitch-bend out-port (channel-of midi) (first-data-byte-of midi) (second-data-byte-of midi)))
                ; System Exclusive messages need to be handled
                ; Midi File Meta messages need to be handled
                (else (sleep 0))))
          (play-midi-stream-iter out-port (stream-rest midi-stream))))
 ```
 
The first procedure returns a thread object that immeadiately begins processing the midi data. The second method is the main loop of this. It interprets the midi data, which is in the format that the ```midi-readwrite``` library uses, and sends the appropriate midi events to the port. It was planned to be able to react to some of the non-sound midi events, which would have required more time.

## 2. Using Procedures to Open and Close Ports

This code allows for opening and closing midi ports, and connecting them to other midi devices and sending and recieving midi data through the ports.

```
(define (make-in-port) (make-rtmidi-in))
(define (make-out-port) (make-rtmidi-out))

(define (list-in-ports in) (rtmidi-ports in))
(define (list-out-ports out) (rtmidi-ports out))

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

(define (close-in-port in-port) (rtmidi-close-port in-port))
(define (close-out-port out-port) (rtmidi-close-port out-port))

(define (send-midi-message port status data1 data2)
  (rtmidi-send-message port (list status data1 data2)))

(define (read-midi-message port)
  (sync port))
```

The ```make-in-port``` and ```make-out-port``` procedures initialize virtual midi ports from RtMidi.

```open-in-port``` and ```open-out-port``` connect ports to other midi ports available on the system by getting the list of available ports and matching a string against the names of those ports.

## 3. Procedural Abstraction

A set of procedures was created for accessing the different parts of a midi event structure. ```midi-readwrite``` processes midi events to a list, ```'(delay (event channel data-1 data-2))``` where delay is how many seconds to delay after the previous event, event is the type of midi message (i.e. 'note-on), channel is the midi channel number from 0 to 15, and data-1 and data-2 are the arguments to the midi message, (i.e. note number and key-hit velocity)

```
(define (delay-of midi-event)
  (car midi-event))

(define (type-of midi-event)
  (list-ref (cadr midi-event) 0))

(define (channel-of midi-event)
  (list-ref (cadr midi-event) 1))

(define (first-data-byte-of midi-event)
  (list-ref (cadr midi-event) 2))

(define (second-data-byte-of midi-event)
  (list-ref (cadr midi-event) 3))
```

## 4. Constructing Procedures Using Lambda Functions to Send Midi data

```
; Procedures for sending midi messages

(define (note-off port channel note) (send-midi-message port (+ 128 channel) note 0))
(define (note-on port channel note velocity) (send-midi-message port (+ 144 channel) note velocity))

; plays a note for a specified length of time in seconds, length can have arbitrary precision
; 24 seems to be the lowest note
; 96 seems to be the highest note
(define (play-note port channel note velocity length)
  (thread (lambda () (note-on port channel note velocity)(sleep length)(note-off port channel note))))

```
There are additional procedures that follow much the same pattern for

```
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
```

