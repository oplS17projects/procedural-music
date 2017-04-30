# Procedural-Music

## Brian Medina
### April 30, 2017

# Overview
This project is designed to be a fully functional interface between Racket and the MIDI standard, with a procedural music generator using MIDI to send notes to external devices and play random music.

My part of the project was implementing the music generator, which is what I will be covering in this report. I also made a small file, rsound-gen, which takes the output from the generator and plays the notes as square waves through the RSound library.

**Authorship note:** All of the code described here is written by me.

# Libraries Used
My portion of the code is very light on library usage. The generator.rkt itself uses only one library:

```
(require racket/stream)
```

This is a standard library that includes basic stream functions.

rsound-gen.rkt uses the following libraries:

```
(require rsound)
(require rsound/piano-tones)
(require portaudio
         ffi/vector)
````

These are all just basic files used with RSound.

# Key Code Excerpts

## 1. Abstraction
 Most procedures are abstracted so as to be readable. Examples:
 ```
 (define make-random-id
  (lambda args
    (cond ((null? args) (random 36 96))
          ((null? (cdr args)) (random (car args)))
          ((null? (cddr args)) (random (car args) (cadr args)))
          (else (error "Invalid arguments for make-random-id")))))
```
 The ```make-random-id``` function, as well as the other randomization functions, ```make-random-velocity``` and ```make-random-duration```, are abstracted when they could easily have been functionally identical as just calls to ```random```. By renaming them, the code is more readable and easier to write.

 ## 2. Recursion
 I used several recursive techniques in making the generator. The procedure that makes each bar of music is defined recursively as follows:

 ```
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
  (iter initial-note 0 beats))
```

This is an iterative recursion procedure that calls itself to build a bar of music until the bar is filled. It does this via the inclusion of an index and a limit. The index is not a typically counted integer index - beats are measured here in as low as 1/16 notes. Functionally, though, it is similar, in that there is a limit and the function ends the list when the limit is reached. If the duration would go over the limit, the function makes a note with truncated duration so as to fit inside the bar. 

There is an edge case where strange durations like a 15/16 note can appear in the case where the remaining duration of the bar is an atypical number and the random new note has a very long duration, but I did not notice the generator making odd sounding music in the many tests I ran on the output.

## 3. Lazy Streams
I also used one of the later learned techniques, lazy computing, when making this generator. Its usage turned out to be fundamental to the project, as it allowed for the demo to run an infinite length of music, as a stream can be infinite, whereas a finite data structure can't be.

```
(define (make-sequence note)
  (define beats (random 3 5))
  (define key (make-key (id-of note)))
  (define bars
    (build-list (random 1 10) (λ (x) (make-random-bar (make-random-note-with-key key) beats))))
  (define (iter bar)
    (cond ((equal? bar '()) (iter (list-ref bars (- (length bars) 1))))
          (else (stream-cons (stream-first bar) (iter (stream-rest bar))))))
  (iter (list-ref bars (- (length bars) 1))))
```

The procedure defines the beats per bar, key, and a list of bars up front. This list has all the bars that will be contained in the eventual stream. The helper function is defined next, and it does several things. It is given a random bar, and it then iterates through that bar and puts it in a stream. When the bar is done with, denoted by the list ender ```'()```, it then chooses another bar to iterate through. Since this uses stream-cons, the cdr of the stream will not be evaluated until played, which means there can be infinite recursions of this note stream.