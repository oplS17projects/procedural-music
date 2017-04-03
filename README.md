# procedural-music

A procedural MIDI music generator in Racket.

### Statement
Procedural-music is a program that will produce MIDI music sequences. Both of us are interested in music and the technology surrounding it, making the project an easy choice.

### Analysis
This project makes use of data objects, abstraction, recursion, and functional programming, all fundamentals of the OPL course.

#### Data Abstraction/Objects
When the MIDI data is generated, functions will be defined to access specific portions of the objects. The data will be stored in a list of note objects, which include the relevant MIDI data of commands, keys, velocities, and whatnot. When output to the protocol that sends these values to the synthesizer, functions will be defined to access the data as well.

#### Recursion/Map/Filter/Reduce
The generation of this data in the note objects will use recursive loops and maps. In addition, recursive loops will be used to check and make sure the data being generated is valid MIDI data.

#### Functional Programming
The random music generation techniques will be programmed in procedures, applied with maps or filters across the note objects. 

### External Technologies

#### Physical
Procedural-music outputs MIDI data, which can be read by a wide array of synthesizers. We are using a keyboard with a built-in synthesizer.

#### Software
The software needed to communicate with an external MIDI device requires drivers and wrappers. We are both working with premade drivers and libraries as well as modifying them to suit our project's needs.

### Deliverable and Demonstration
When we are finished, we will have a program that can take commands and produce music. The specific lexicon and implementation of these commands is yet to be determined, but at the very least, a procedure to produce a certain length of music will be a part of the final deliverable.

### Evaluation of Results
Conversion between the procedural music generator's output and the MIDI standard is the goal here; when we can input some dummy data and have it perform as expected, this part of the project will be done.

The evaluation of the actual procedures that generate the music is more nebulous. The goal would be to have it produce something musical, but at the very least it should produce something to play.

## Architecture Diagram
![Architecture Diagram](/Procedural-Music_Components.jpg?raw=true "Architecture Diagram")

Discription of the architecture diagram goes here.

## Schedule

### First Milestone (Sun Apr 9)
Which portion of the work will be completed (and committed to Github) by this day? 

### Second Milestone (Sun Apr 16)
Which portion of the work will be completed (and committed to Github) by this day?  

### Public Presentation (Mon Apr 24, Wed Apr 26, or Fri Apr 28 [your date to be determined later])
What additionally will be completed before the public presentation?

### Samuel Toups @SamuelToups
Will work on the racket-midi interface and the communication with the physical synth-keyboard.

### Brian Medina @probune
Will provide the procedures that produce the music.
