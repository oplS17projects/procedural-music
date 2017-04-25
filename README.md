# procedural-music

A procedural MIDI music generator in Racket.

### Statement
Procedural-music is a program that produces MIDI music sequences. Both of us are interested in music and the technology surrounding it, making the project an easy choice.

### Analysis
This project makes use of data objects, abstraction, recursion, and functional programming, all fundamentals of the OPL course.

#### Data Abstraction/Objects
When the MIDI data is generated, functions are defined to access specific portions of the objects. The data is stored in a list of note objects, which include the relevant MIDI data of commands, keys, velocities, and whatnot. When output to the protocol that sends these values to the synthesizer, functions can access the data as well.

#### Recursion/Map/Filter/Reduce
The generation of this data in the note objects uses recursive loops and maps.

#### Functional Programming
The random music generation techniques are programmed in procedures, applied with procedures applied using streams. 

### External Technologies

#### Physical
Procedural-music outputs MIDI data, which can be read by a wide array of synthesizers. We are using a keyboard with a built-in synthesizer that supports the General MIDI standard.

#### Software
This project depends on the following libraries:

rtmidi:         racket wrapper for the RtMidi library
RtMidi:         c++ library for interfacing with software and hardware midi devices
midi-readwrite: racket library for processing midi data

Using these three libraries, the software can communicate with any external MIDI device. Some of the libraries needed modifying to suit our project's needs, most noteably the install scripts for rtmidi and RtMidi needed some slight adjustments, and the midi-readwrite library was found to have a bug which caused midi channels 9-16 to be mapped to channels 1-8.

### Deliverable and Demonstration
The program can take commands and produce music. It fully exports MIDI data commands to an external MIDI device or internal MIDI program, and the generator produces music that is made of completely random notes.

### Evaluation of Results
Conversion between the procedural music generator's output and the MIDI standard was the goal here; we can input some dummy data and it performs as expected,meaning this part of the project is done.

The evaluation of the actual procedures that generate the music was more nebulous. The goal was to have it produce something musical, but at the very least it produces something to play.

## Architecture Diagram
![Architecture Diagram](/Procedural-Music_Components.jpg?raw=true "Architecture Diagram")

The midi generator code sends the data to the rtmidi interface part of the code. The rtmidi interface part of the code then takes that midi data and calls functions of the rtmidi racket library, which is a wrapper for a cross-platform midi library written in c++98. The c++ library connects to the os-specific audio driver software, in the case of linux this is Jackd and alsa, and sends the midi data to the midi device. Jackd and alsa provide virtual midi cables, allowing the rtmidi library to communicate with any software or hardware midi synth.

## Schedule

### First Milestone (Sun Apr 9)
Generator: can generate some kind of notes.

Interface: Be able to send midi notes to a synthesizer and provide methods for sending midi data.

### Second Milestone (Sun Apr 16)
Generator: can generate some kind of notes which can communicate with the Interface with possible errors.

Interface: Be able to send instrument select and other midi control messages to the synthesizer.

### Public Presentation (Wed Apr 26)
Generator: fully generates notes and outputs to the Interface with minimal errors.

Interface: send midi data from the generator to the synth in real time.

### Samuel Toups @SamuelToups
Worked on the racket-midi interface and the communication with the physical synth-keyboard.

### Brian Medina @probune
Provided the procedures that produce the music.
