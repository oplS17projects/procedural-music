This project depends on the following libraries:

rtmidi: a racket wrapper for the RtMidi library, which can be found [here](http://docs.racket-lang.org/rtmidi/index.html)

RtMidi: a c++ library for interacting with midi devices, rtmidi depends on version 2.1.0, which can be found [here](http://www.music.mcgill.ca/~gary/rtmidi/release/)

midi-readwrite: a racket library for accessing midi data and files, which can be found [here](https://docs.racket-lang.org/midi/index.html)


Installing and configuring the 3rd party libraries for this project is not quite straight forward.

For the most part the instructions for installing the three can be followed as is, except in the following cases.


see the racket documentation for the rtmidi package for additional install steps

the following are changes that need to be made to the Makefile for the rtmidi
package for racket.

system specific install commands are still needed for mac and windows

the following lines need to be added

unix: CXXFLAGS += -D__UNIX_JACK__ -ljack

unix: LDFLAGS += -ljack -lpthread

unix: wrap-rtmidi.so

install-unix: unix

cp wrap-rtmidi.so /usr/lib/

install-linux: linux

cp wrap-rtmidi.so /usr/lib/


When building the library, run make install-[OS], where [OS] is the operating system you are on.



the following are changes that need to be made to the Makefile for RtMidi-2.1.0
the following line needs to be commented out

install --mode=644 RtMidi.h RtError.h $(PREFIX)/include


UPDATE: My fix has been pulled

~~until the racket library midi-readwrite is updated with my fix, the following file needs to be changed.~~

~~/home/USERNAME/.racket/6.8/pkgs/midi-readwrite/midi-readwrite/midi-read.rkt~~

~~the following changes need to be made~~

~~(define channel (bitwise-and #x7 next-byte))~~

~~needs to be changed to~~

~~(define channel (bitwise-and #xf next-byte))~~

~~and~~

~~(define channel (bitwise-and #x7 prior-event-type-byte))~~

~~needs to be changed to~~

~~(define channel (bitwise-and #xf prior-event-type-byte))~~

~~this is to fix an issue where midi channels 9-16 were being mapped onto midi channels 1-8~~




for info on setting up midi devices and software on linux, refer to
http://tedfelix.com/linux/linux-midi.html

for info on setting up midi devices and software on windows, refer to
http://donyaquick.com/midi-on-windows/

need to find a link to a resource for setting up midi on Mac
