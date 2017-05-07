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

Five examples are shown and they are individually numbered. The titles of each section highlight the course concepts embodied.

## 1. Playing a Stream of Midi data

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
 
The first procedure returns a thread object that immeadiately begins processing the midi data. The second method is the main loop of this. It interprets the midi data, and which is in the format that the ```midi-readwrite``` library uses, and sends the appropriate midi events to the port. It was planned to be able to react to some of the non-sound midi events, but there wasn't enough time.
 
## 2. Selectors and Predicates using Procedural Abstraction

A set of procedures was created to operate on the core ```drive-file``` object. Drive-files may be either
actual file objects or folder objects. In Racket, they are represented as a hash table.

```folder?``` accepts a ```drive-file```, inspects its ```mimeType```, and returns ```#t``` or ```#f```:

```
(define (folder? drive-file)
  (string=? (hash-ref drive-file 'mimeType "nope") "application/vnd.google-apps.folder"))
```

Another object produced by the Google Drive API is a list of drive-file objects ("```drive#fileList```"). 
When converted by the JSON library,
this list appears as hash map. 

```get-files``` retrieves a list of the files themselves, and ```get-id``` retrieves the unique ID
associated with a ```drive#fileList``` object:

```
(define (get-files obj)
  (hash-ref obj 'files))

(define (get-id obj)
  (hash-ref obj 'id))
```
## 3. Using Recursion to Accumulate Results

The low-level routine for interacting with Google Drive is named ```list-children```. This accepts an ID of a 
folder object, and optionally, a token for which page of results to produce.

A lot of the work here has to do with pagination. Because it's a web interface, one can only obtain a page of
results at a time. So it's necessary to step through each page. When a page is returned, it includes a token
for getting the next page. The ```list-children``` just gets one page:

```
(define (list-children folder-id . next-page-token)
  (read-json
   (get-pure-port
    (string->url (string-append "https://www.googleapis.com/drive/v3/files?"
                                "q='" folder-id "'+in+parents"
                                "&key=" (send drive-client get-id)
                                (if (= 1 (length next-page-token))
                                    (string-append "&pageToken=" (car next-page-token))
                                    "")
;                                "&pageSize=5"
                                ))
    token)))
```
The interesting routine is ```list-all-children```. This routine is directly invoked by the user.
It optionally accepts a page token; when it's used at top level this parameter will be null.

The routine uses ```let*``` to retrieve one page of results (using the above ```list-children``` procedure)
and also possibly obtain a token for the next page.

If there is a need to get more pages, the routine uses ```append``` to pre-pend the current results with 
a recursive call to get the next page (and possibly more pages).

Ultimately, when there are no more pages to be had, the routine terminates and returns the current page. 

This then generates a recursive process from the recursive definition.

```
(define (list-all-children folder-id . next-page-token)
  (let* ((this-page (if (= 0 (length next-page-token))
                      (list-children folder-id)
                      (list-children folder-id (car next-page-token))))
         (page-token (hash-ref this-page 'nextPageToken #f)))
    (if page-token
        (append (get-files this-page)
              (list-all-children folder-id page-token))
        (get-files this-page))))
```

## 4. Filtering a List of File Objects for Only Those of Folder Type

The ```list-all-children``` procedure creates a list of all objects contained within a given folder.
These objects include the files themselves and other folders.

The ```filter``` abstraction is then used with the ```folder?``` predicate to make a list of subfolders
contained in a given folder:

```
(define (list-folders folder-id)
  (filter folder? (list-all-children folder-id)))
```

## 5. Recursive Descent on a Folder Hierarchy

These procedures are used together in ```list-all-folders```, which accepts a folder ID and recursively
obtains the folders at the current level and then recursively calls itself to descend completely into the folder
hierarchy.

```map``` and ```flatten``` are used to accomplish the recursive descent:

```
(define (list-all-folders folder-id)
  (let ((this-level (list-folders folder-id)))
    (begin
      (display (length this-level)) (display "... ")
      (append this-level
              (flatten (map list-all-folders (map get-id this-level)))))))
```
