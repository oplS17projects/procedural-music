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

### Samuel Toups @SamuelToups
Will work on the racket-midi interface and the communication with the physical synth-keyboard.

### Brian Medina @probune
Will provide the procedures that produce the music.

## Still to do:

### Deliverable and Demonstration
Explain exactly what you'll have at the end. What will it be able to do at the live demo?

What exactly will you produce at the end of the project? A piece of software, yes, but what will it do? Here are some questions to think about (and answer depending on your application).

Will it run on some data, like batch mode? Will you present some analytical results of the processing? How can it be re-run on different source data?

Will it be interactive? Can you show it working? This project involves a live demo, so interactivity is good.

### Evaluation of Results
How will you know if you are successful? 
If you include some kind of _quantitative analysis,_ that would be good.

## Architecture Diagram
Upload the architecture diagram you made for your slide presentation to your repository, and include it in-line here.

Create several paragraphs of narrative to explain the pieces and how they interoperate.

## Schedule
Explain how you will go from proposal to finished product. 

There are three deliverable milestones to explicitly define, below.

The nature of deliverables depend on your project, but may include things like processed data ready for import, core algorithms implemented, interface design prototyped, etc. 

You will be expected to turn in code, documentation, and data (as appropriate) at each of these stages.

Write concrete steps for your schedule to move from concept to working system. 

### First Milestone (Sun Apr 9)
Which portion of the work will be completed (and committed to Github) by this day? 

### Second Milestone (Sun Apr 16)
Which portion of the work will be completed (and committed to Github) by this day?  

### Public Presentation (Mon Apr 24, Wed Apr 26, or Fri Apr 28 [your date to be determined later])
What additionally will be completed before the public presentation?
