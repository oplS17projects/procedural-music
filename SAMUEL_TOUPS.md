# Procedural-Music

## Samuel Toups
### April 30, 2017

# Overview

The purpose of this project was to create a Racket-MIDI interface, as well as a procedural music generator that would use the interface to control a MIDI device to produce sound.

My contribution to this project was the Racket-MIDI interface, which provides a number of methods for interacting with MIDI devices.


**Authorship note:** All of the code described here was written by myself.

# Libraries Used
The code uses four libraries:

```
(require rtmidi)
(require midi-readwrite)
(require control)
(require racket/stream)
```

* The ```rtmidi``` library provides a racket wrapper for the c++ library RtMidi, allowing for sending and recieving midi data.
* The ```midi-readwrite``` library provides utilities for interacting with midi data.
* ```control``` provides flow-control structures such as ```while``` loops.
* ```racket/streams``` add lazily evaluated sequences of data.


# Key Code Excerpts

Here is a discussion of the most essential procedures, including a description of how they embody ideas from 
UMass Lowell's COMP.3010 Organization of Programming languages course.

Five examples are shown and they are individually numbered. 

## 1. play-midi-stream-iter

The following code is at the core of the project's ability to play midi data. It takes a midi port and a stream of midi events and sends midi messages to the port, spacing out the events in time so that the events don't all happen at once.

```play-midi-stream-iter``` sleeps for the duration between the current event and the previous, then sends out the event. It then recursively calls itself on the remainder of the stream.

```play-midi-stream``` wraps this process in a thread to allow other operations to continue in parallel.

Lazy Evaluation using streams and Tail-Recursion are two of the major themes of the course.


```
(define (play-midi-stream out-port midi-stream)
  (thread (λ () (play-midi-stream-iter out-port midi-stream))))

(define (play-midi-stream-iter out-port midi-stream)
  (if (not (stream-empty? midi-stream))
      (let ([midi (stream-first midi-stream)])
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


## 2. Abstraction

Abstraction is another major theme of the course. By using abstraction to access elements of the midi event, the code is much more readable than if ```car``` and ```cdr```, or any of their variations, had been used, and a lot easier to write.

```
;; accessors for midi events

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

### 3. Ports

The code below provides access to the methods of the ```rtmidi``` library. ```open-in-port``` and ```open-out-port```, which allow a port to be found by matching a string against the names of the available ports. Also note ```send-midi-message``` and ```read-midi-message```.


```
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
```

