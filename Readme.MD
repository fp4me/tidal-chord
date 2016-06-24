tidal-chord
=======

tidal-chord is an add-on to the amazing live coding project tidal 


chord format
-----------
Chord format is inspired of the ableton live glasgow tool ( https://github.com/asb2m10/glasgow)

Note name:  A B C D E F G with # and b.

Mode Name:  M(Major) m(Minor) Ionian Dorian Phrygian Lydian Myxolidian Aeolian Locrian 

Scale Degre(optional): ° [1-7] degre in scale where the chord is applied  

Neighbour key tonality(optional) : +(one step in circle of fifth) or -(one step in circle of fourth)

Relative Mode(optional) : > [Mode Name] convert scale to the relative mode  

Chord Definition: | [Chord Name or Chord degres sep by - ] 
                     Chord Name can be one of the following : Maj | Min | Maj7 | Min7 | Dom7 | Maj6 | Min6 | Five | Sus4 | Sus2 | MajorAdd2 | MinorAdd2 | Dom9 | Dom13 | Min9 | Maj9 | Maj13 | Min11
                     
Invertion : ! 

       note name
       |     scale degre
       |     |   
       |     |  mode name 
       |     |  |     relative mode name
       |     |  |     |   chord is a Maj chord
       |     |  |     |   |     makes invertions to chords (!! makes 2 invertions)
       |     |  |     |   |     | 
       C 3 ° 2  M + > m | Maj   !!
       C 3 ° 2  M + > m | 3 - 7 !! 
         | |      | |     |   |   
         | |      | |     |   |   
         | |      | |     |---|
         | |      | |       |   
         | |      | |       chord is defined by chord degre 3 and 7
         | |      | | 
         | |      | force the parser to read the relative mode
         | |      neighbour key tonality (+ make one step in the circle of fifth)  
         | | 
         | force the parser to read the scale degre
         octave value 

     chordToNotes "C3M|3-7" = [60,64,71]
     chordToNotes "C3M|Min9!!" = [63,70,72,74,79]
     chordToNotes "C3°4M>m|Min!!" = [77,86,93]

## Usage

This examples use also the tidal-midi package 

```haskell
:load "TidalChord.hs"
import Sound.Tidal.MIDI.Context

devices <- midiDevices
m1 <- midiStream devices "VMidi 1" 1 synthController

m1 $ midiChord "~ C2M*3 ~ C2°5M*2 C3°7M/2" # velocity (Sound.Tidal.Context.scale 0.1 0.6 (slow 8 $ sine1))

m1 $ midiChord "C3M- C3°7M-*2 C2°5M-|3-5-9/2 "




