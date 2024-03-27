## Binary entry format
```
%TTTTxxxx
 ||||++++--- subspecifier
 ++++------- Type
```
Types between 4-15 are notes:
     The value is the number of half-steps above the base note of the current octave, plus 4.
     The subspecifier is a reference to the note's length, set via the length table setter.

Length: Stored as however many 256ths of a measure a given length is
     16 locations to store this in

Remaining types:
     0: Other
         Subspecifier type:
             1xxx: Loop
                 xxx: Loop index
                 Byte data: Hi bit: setloop/ goto
                     If setloop, unsigned offset from loop directive to destination
                     If goto, number of times to loop. 0 indicates infinite looping
             01xx: Tone change
                 xx: Channel 1/2 duty
                 Byte data: Channel 3 wave index
             0011: Tempo change
                 Byte data: BPM, 0 to append next note (tie)
             0010: Auto Length (for stacatto)
                 Byte data: value to set Counter to, 0 to disable
                     High two bits omitted when not channel 3
                     If control channel, set global panning
             0001: Envelope
                 Byte data: Envelope data
                     If control channel, Master volume data
             0000: Sweep
                 Byte data: Sweep data
                     If control channel, toggle a channel's panning
                         Bits 0-1: Affected channel
                         Bit 2:    L/R select
                         Bit 3:    If 0, output is toggled. Otherwise, bit 4 applies
                         Bit 4:    Value to apply (1==on)
     1: Length change
         Subspecifier: Length index to change
         Byte data: New length value
     2: Octave
         Subspecifier: Octave no.
     3: Rest
         Subspecifier: length

Header format:
For each channel:
2 bytes =  Channel start offset from beginning of file


MML text format:
All entries are a character with a number following. Valid characters are:
    A-G:    Note declaration
    R:      Rest
    V:      Volume
    S:      Sweep/Single Stereo
    T:      Tempo
    O:      Octave
    I:      Instrument
    H:      Stacatto/Global Stereo
    []:     Loop
    X:      Channel
    ^:      Imply
    @:      Macro
    
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
    R: Rest. Plays silence
        Numerals following are the same as any of A-G.
    V: Envelope setting
        Numeral following is the value to feed into the envelope register. If channel 3, the top two bits of volume are taken and fed into channel shifting, with reasonable adjustment.
        A dot following allows specifying volume magnitude and shift separately
        e.g. V15.0 sets volume to maximum with no envelope
    V: Global Volume setting (Control Channel only)
        Numeral following is the value to feed into the global volume register.
        A reminder that the volume only goes up to 7 per side
        A dot following splits between left and right output
    S: Sweep setting
        Numeral following is the value to feed into the sweep register. If not channel 1, this directive is ignored.
        A dot following allows specifying the time and the magnitude seperate
        e.g. S$77 sets sweep to a slow descent
    S: Single Panning (Control Channel only)
        Numeral following is a compound value, specifying one of one voice channel's outputs, and enabling, disabling, or toggling it. 
    T: Tempo setting
        Numeral following is the new tempo, in BPM. Maximum BPM is 255
    O: Set Octave
        Numeral following is the new octave, as per the octave table set in the header.
    I: Set instrument
        Numeral following is the wave duty to switch to, or a channel 3 wave index. If channel 4, this directive is ignored. A dot with a numeral following specifies a seperate index for channel 3.
    H: Set actual note length (stacatto)
        Numeral following is the value fed into the Sound Length register. When this value is 0, Sound Length disabled
    H: Set Global Panning (Control Channel only)
        Numeral following is the value fed into the Channel Output register, specifying which channels go into the left and right outputs
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
Note on loops:
    A given bracket pairs with the nearest matching bracket i.e. all loops are fully contained
    If one of these has an index specification and the other doesn't, the other inherits that index
    However, if both brackets have index specifications, neither are overwritten
    This is so that you can have overlapping loops, but you are responsible for index allocation
    Compilers will also assume fully contained loops, so anything affected by looping (e.g. GB's length messages) will be screwed over
    Additionally, setting index to max is invalid
