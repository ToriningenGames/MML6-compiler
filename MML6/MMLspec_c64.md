## General principles

Length: Stored as however many 256ths of a measure a given length is
     16 locations to store this in

Percussion mode:
     An alternative mode intended for percussion, automating all voice registers and functions, and turning each "note" into a customizable instrument.
     After disabling this mode, all state for the channel is undefined.

Control Channel:
     The first channel specified (`X0` in the text source) is treated specially as a *control channel*, in charge of global sound state. For the Commodore 64, this amounts to the filters and volume controls

## Binary entry format
```
%TTTTxxxx
 ||||++++--- subspecifier
 ++++------- Type
```
Types between 4-15 are notes:
     The value is the number of half-steps above the base note of the current octave, plus 4.
     The subspecifier is a reference to the note's length, set via the length table setter.

Remaining types:
     1: Length change
         Subspecifier: Length index to change
         Byte data: New length value
     2: Loop
         Subspecifier: Loop index
             Byte data: Hi bit: setloop/ goto
                 If setloop, unsigned offset from loop directive to destination
                 If goto, number of times to loop. 0 indicates infinite looping
     3: Rest
         Subspecifier: length
     0: Other
         Subspecifier type:
             1xxx: Octave
                 xxx: New Octave
                 No byte data
             0011: Tempo change
                 Changes the global tempo
                 Byte data: BPM
             0000: Tie/Slur
                 Makes the next note not retrigger the channel, and instead continue playing while the frequency switches. No following data byte. This directive must immediately follow the note to tie.
         Music Channels:
             0100: Control
                 Controls the inter-channel syncing and Percussion mode
                 Data:
                 ```%00003RSP
                         |||+-- Percussion mode
                         ||+--- Sync
                         |+---- Ring Mod
                         +----- Channel 3 Direct Out enable toggle
                 ```
             0101: ADSR/Envelope
                 Data:
                 ```%AAAADDDD SSSSRRRR
                     |||||||| ||||++++--- Release
                     |||||||| ++++------- Sustain
                     ||||++++------------ Decay
                     ++++---------------- Attack
                 ```
             011x: Waveform/Phase
                 x: Test toggle
                     When set, flashes the test bit while changing the waveform
                 Data:
                 ```%NSWTPPPP PPPPPPPP
                     ||||++++-++++++++--- Square wave duty cycle
                     |||+---------------- Triange Wave enable
                     ||+----------------- Saw Wave enable
                     |+------------------ Square Wave enable
                     +------------------- Noise enable
                 ```
             0010: Virbrato
                 Causes a frequency wobble.
                 Data:
                 ```%IIIISSSS
                     ||||++++--- Speed
                     ++++------- Intensity
                 ```
             0001: Sweep
                 Causes a sweep up/down at a given speed.
                 Byte data: The signed frequency difference to apply
         Control Channel:
             0100: Filter Enables
                 Enables the filter effects on the selected channels
                 Data:
                 ```%0000E321
                         |||+--- Channel 1 Filter
                         ||+---- Channel 2 Filter
                         |+----- Channel 3 Filter
                         +------ External Input Filter
                 ```
             0101: Global Volume
                 Byte data: The output volume (between 0 and 15 incl.)
             0110: Filter Mode
                 Customizes the type of filter applied
                 Data:
                 ```%0HBLRRRR
                      |||++++--- Resonance
                      ||+------- Enable Low Pass
                      |+-------- Enable Band Pass
                      +--------- Enable High Pass
                 ```
             0111: ?
             0010: Tremolo
                 Causes a volume wobble.
                 Data:
                 ```%IIIISSSS
                     ||||++++--- Speed
                     ++++------- Intensity
                 ```
             0001: Filter Frequency Sweep
                 Sweeps the filter frequency up/down at a given speed
                 Byte data: The signed frequency difference to apply

Header format:
For each channel:
2 bytes =  Channel start offset from beginning of file


## MML text format
All entries are a character with a number following. Valid characters are:
    A-G:    Note declaration
    R:      Rest
    O:      Octave
    T:      Tempo/Tie
    []:     Loop
    X:      Channel
    ^:      Imply
    @:      Macro
    V:      Volume/ADSR Envelope
    S:      Sweep
    W:      Virbrato/Tremolo (Wobble)
    I:      Instrument/Filter Enables
    Q:      Voice Control/Filter Control
    
Numbers immediately follow valid characters, no space, in one of these formats:
    [0-9]:      Decimal literal
    %[0-1]:     Binary literal
    $[0-9A-F]:  Hexadecimal literal (requires trailing whitespace)
    .:          Special character between numerical literals. Allows specificity
The dot character takes precedance over base specifiers i.e. '$F.13' is equal to '15.13'

Meanings:
    #: Comment string. Continues until end of line
    A-G: At the current octave, play a note of this pitch.
        Should the character following be a + or a -, the note shall be sharp or flat, respectively.
        The numeral following specifies length as the denominator of the note
        e.g. C4 plays a quarter note at the pitch of C
        A dot following is similar to a tie, or a dotted note, in that it specifies the numerator of the played note
        e.g. D8.3 plays a dotted quarter note, or three tied eighth notes
    O: Set Octave
        Numeral following is the new octave. Octaves are based on A as the base note.
    R: Rest. Plays silence
        Numerals following are the same as any of A-G.
    T: Tempo setting/Tie
        Numeral following is the new tempo, in BPM. Maximum BPM is 255. If the BPM specified is 0, the directive instead acts as a tie or slur: the following note doesn't cause a new attack
    [: Loop Start
        Numeral following is a label ID to loop back to.
        A dot following allows specifying the particular index to loop with.
    ]: Loop Go
        Numeral following is the number of times to loop. When this value is 0, loop forever.
        Loop brackets are linked by nearest pair based on actual location
        A dot following allows specifying the particular index to loop with.
    :: Loop Label
        Numeral following is an ID for this label.
        This is the loop destination for preceding loop starts having a matching ID
    X: Channel start
        Defines at what directive a given channel should start playing
        Numeral following is which channel is now beginning.
        Channel 0 is the control channel
    ^: Imply
        Numeral following is what numeral follows directives that have no numeral after them when mandatory. This applies to ALL directives (except shifts)!
    @: Macro
        Allows substitution of commonly-used phrases of music.
        Numeral following is the macro identifier.
        Macros may not be recursive
        Implications are done before macro substitution
        Macros are defined before all channels. Therefore, channels may not start within macros.
        Macros are terminated with another macro declaration or a channel declaration
        An optional dot and numeral indicate a number of directives to skip in reading/ pasting
            Note: Implys are not counted in this number
    <>: Octave shift
        Statically move down/up an octave, respectively. Equivalent to an O directive
        An optional numeral following specifies how many octaves to shift
    V: ADSR Envelope (Music Channel)
        Controls the ADSR envelope of this channel. Following is a sixteen-bit value specifying each 4 bit component, left to right
    V: Volume (Control Channel)
        Controls the global volume, to any value between 0 and 15
    S: Sweep (Music Channel)
        Adds a frequency sweep. Takes an optional sign before the base specifier to specify which direction to sweep. More extreme values result in faster sweeps. Signed 8 bit value.
    S: Filter Sweep (Control Channel)
        Adds a sweep to the resonance frequency of the filter. Takes an optional sign before the base specifier to specify which direction to sweep. More extreme values result in faster sweeps. Signed 8 bit value.
    I: Set instrument (Music Channel)
        Sets the instrument to use with this channel. The value is a bitfield of 5 bits, enabling in order from lo to hi the waves for Triangle, Saw, Square, Noise, and Test Toggle. Setting test toggle sets the test bit while the wave is changed, resulting in a split-second of no output, and resetting the phases of the waves. An optional specifier is a 12 bit value for the duty cycle of the square wave. If it is not present, the duty will be 50%.
    I: Filter Enables (Control Channel)
        Enables/Disables the filter for the various channels and external input. Bits from lo to hi are Channel 1, 2, 3, and external.
    W: Virbrato (Music Channel)
        Wobbles the note frequency over time. The high 4 bits control the intensity of the virbrato (higher is more), and the low 4 bits control the speed of the virbrato (higher is slower). A dot specifier may separate the two.
    W: Tremolo (Control Channel)
        Wobbles the global volume over time. The high 4 bits control the intensity of the tremolo (higher is more), and the low 4 bits control the speed of the tremolo (higher is slower). A dot specifier may separate the two.
    Q: Control (Music Channel)
        Sets control values for the given channel. The value is a 4-bit bitfield, whose effects from lo to hi are Percussion mode, Sync, Ring Mod, Ch3 Direct Out Enable. Sync and Ring Mod enable the appropriate bits for the given Channel. Ch3 Direct Out Enable toggles whether Channel 3's unfiltered output is audible. Only Channel 3 may affect this bit. Percussion mode changes how notes are played, from using the user-specified instrument controls to using player-side defined instruments for each note.
    Q: Filter Control (Control Channel)
        Customizes the global filter. The values provided set resonance as a 4-bit value, and bits enabling Low, Band, and High pass around this resonance. When 1 value is specified, the low 4 bits are the resonance, and the upper 4 bits specify the passes, from lo to hi respectively. When a dot specifier is present, the first value specifies the passes as before, and the second value specifes the resonance.
Note on loops:
    A given bracket pairs with the nearest matching bracket i.e. all loops are fully contained
    If one of these has an index specification and the other doesn't, the other inherits that index
    However, if both brackets have index specifications, neither are overwritten
    This is so that you can have overlapping loops, but you are responsible for index allocation
    Compilers will also assume fully contained loops, so anything affected by looping (e.g. length messages) will be screwed over
    Additionally, setting index to max is invalid
