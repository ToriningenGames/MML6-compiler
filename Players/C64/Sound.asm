.INCLUDE IO.inc

;RAM use
;ZP Defaults fit with KERNAL; define BasicCompat to avoid overwriting BASIC vars
.DEFINE DIVAREA         $8B     ;5 bytes ZP
.DEFINE NoteTable       $FB     ;2 bytes ZP
.DEFINE Temp            $2B     ;5 bytes ZP
.DEFINE SoundState      $CE00   ;2 pages, page aligned

.STRUCT LoopLength
notelen db
loopcnt db
loopptr dw
.ENDST

.STRUCT MusicChannel
;Shadows
freq    dw  ;Channel 0 Filter Freq
phase   dw  ;Channel 0 Mode (upper nibble), Channel 0 Vol (whole byte, use upper nibble)
control db  ;Channel 0 None
adsr    dw  ;Channel 0 Res/Filt
;Actual meaningful registers
now     dw
remain  db
octave  db
wobble  db
wobtime db
sweep   db
system  db
  ;%TP000UWS
  ; ||   ||+--- Frequency sweep active
  ; ||   |+---- Virbrato/Tremolo active
  ; ||   +----- Virbrato/Tremolo Up/Down flag
  ; |+--------- Percussion Mode
  ; +---------- Next note is tied
.ENDST

.ENUM SoundState
channel         INSTANCEOF MusicChannel 4
SlowFlag        db
Delay           db
Clock           dw
.ENDE
.ENUM SoundState + $0100
loops   INSTANCEOF LoopLength 4*16
.ENDE

;Useful Constants
.DEFINE NtscTimer       $0EA157
.DEFINE PalTimer        $0E1816
.DEFINE DreanTimer      $0EA3F7

.SECTION "Freq Table" FREE BITWINDOW 8
;     A     A+    B     C     C+    D     D+    E     F     F+    G     G+
NtscFreq:
 .dw $70C8,$777D,$7E97,$861E,$8E18,$968B,$9F7F,$A8FB,$B307,$BDAC,$C8F4,$D4E7
PalFreq:
 .dw $7512,$7C08,$8368,$8B39,$9380,$9C45,$A590,$AF68,$B9D6,$C4E3,$D099,$DD00
DreanFreq:
 .dw $70B3,$7767,$7E81,$8606,$8DFE,$9670,$9F62,$A8DC,$B2E7,$BD8A,$C8D0,$D4C0
.ENDS

.SECTION "Percussion" FREE
;      Freq  ADSR  Phase Control  Time
.table dw,   dw,   dw,   db,      db
PercData:
 .row $0000,$0000,$0000,$00,     $00    ;A  (None)
 .row $0000,$0000,$0000,$00,     $00    ;A+ (None)
 .row $0000,$0000,$0000,$00,     $00    ;B  (None)
 .row $0000,$0000,$0000,$00,     $00    ;C  (None)
 .row $0000,$0000,$0000,$00,     $00    ;C+ (None)
 .row $0000,$0000,$0000,$00,     $00    ;D  (None)
 .row $0000,$0000,$0000,$00,     $00    ;D+ (None)
 .row $0000,$0000,$0000,$00,     $00    ;E  (None)
 .row $0000,$0000,$0000,$00,     $00    ;F  (None)
 .row $0000,$0000,$0000,$00,     $00    ;F+ (None)
 .row $0000,$0000,$0000,$00,     $00    ;G  (None)
 .row $0000,$0000,$0000,$00,     $00    ;G+ (None)
.ENDS

.SECTION "Sound Setup" FREE

SoundInit:
;Figure out the clock speed
;Use the clock to time the CIA
;We don't know if we're NTSC, PAL, or Drean; this leaves 6 possibilities
  SEI
  LDA #$00
  STA $DD08
-
  CMP $DD08
  BEQ -
  LDA #$FF
  STA $DD04
  STA $DD05
  LDA #%00010001
  STA $DD0E
  LDA $DD08
-
  CMP $DD08
  BEQ -
  ;Drean and NTSC are real close; a little nudge is needed
  LDA $DD04
  ASL A
  LDA $DD05
  ADC #$00
  CLI
  ;Possibilities:
          ;60 Hz, PAL sys: about $7f
          ;50 Hz, PAL sys: about $32
          ;60 Hz, NTSC sys: about $71
          ;50 Hz, NTSC sys: about $21
          ;60 Hz, Drean sys: about $70
          ;50 Hz, Drean sys: about $20
;Since we have the clock, take this opportunity to set the note table too
  CMP #$7F
  BEQ Pal
  CMP #$32
  BEQ Pal
  CMP #$70
  BEQ Drean
  CMP #$20
  BEQ Drean
;High byte of clock is always $0E
Ntsc:
  LDA #>NtscFreq
  STA NoteTable+1
  LDA #(NtscTimer & $FF)
  LDX #((NtscTimer & $FF00) >> 8)
  ;LDY #((NtscTimer & $FF0000) >> 16)
  LDY #<NtscFreq
  BNE SysCommon
Pal:
  LDA #>PalFreq
  STA NoteTable+1
  LDA #(PalTimer & $FF)
  LDX #((PalTimer & $FF00) >> 8)
  ;LDY #((PalTimer & $FF0000) >> 16)
  LDY #<PalFreq
  BNE SysCommon
Drean:
  LDA #>DreanFreq
  STA NoteTable+1
  LDA #(DreanTimer & $FF)
  LDX #((DreanTimer & $FF00) >> 8)
  ;LDY #((DreanTimer & $FF0000) >> 16)
  LDY #<DreanFreq
SysCommon:
  STA.w Clock
  STX.w Clock+1
  ;STY Clock+2
  STY NoteTable
  
;Make sure the timer is off
  LDA #$00
  STA.w CIA2.TimControlB
.IFNDEF ShareNMI
;Hook NMI. We assume nothing important was there.
  LDA #<NMIHook
  LDX #>NMIHook
.IFDEF KernalCompat
;Hijack the NMI from the Kernal
  STA $0318
  STX $0319
.ELSE
;Not using the Kernal; patch NMI directly
  STA $FFFA
  STX $FFFB
.ENDIF
.ENDIF
  RTS

.ENDS

.SECTION "Sound Functions" FREE

;When not using a handler
NMIHook:
  PHA
;Is it us?
  LDA.w CIA2.IntControl
  AND #%00000010
  BNE +
  PLA
  RTI
+
;It's us.
  TXA
  PHA
  TYA
  PHA
  JSR PlayTick
  PLA
  TAY
  PLA
  TAX
  PLA
  RTI

;A,X -> New Song
NewSong:
  ;Store the new song pointer
  STX Temp
  STA Temp+1
  ;Reset the SID
  LDA #$00
  LDX #24
-
  STA SIDch,X
  DEX
  BPL -
  ;Wipe memory
  LDX #$00
-
  STA SoundState,X
  STA SoundState+$0100,X
  DEX
  BNE -
  ;Load in the channel pointers
  CLC
  LDY #$00
  LDA (Temp),Y
  ADC Temp
  STA channel.1.now
  INY
  LDA (Temp),Y
  ADC Temp+1
  STA.w channel.1.now+1
  INY
  LDA (Temp),Y
  ADC Temp
  STA channel.2.now
  INY
  LDA (Temp),Y
  ADC Temp+1
  STA.w channel.2.now+1
  INY
  LDA (Temp),Y
  ADC Temp
  STA channel.3.now
  INY
  LDA (Temp),Y
  ADC Temp+1
  STA.w channel.3.now+1
  INY
  LDA (Temp),Y
  ADC Temp
  STA channel.4.now
  INY
  LDA (Temp),Y
  ADC Temp+1
  STA.w channel.4.now+1
  ;Make it play over the next ticks
  ;Stagger the channels so nothing's overwhelmed
  LDY #$01
  STY channel.1.remain
  INY
  STY channel.2.remain
  INY
  STY channel.3.remain
  INY
  STY channel.4.remain
  ;Set the tempo to a fine default
  LDA #120
  JMP SetTempo
-
  RTS

PlayTick:
;Run slower for lower tempos
  DEC Delay
  BNE -
  LDA SlowFlag
  STA Delay
.IFDEF BasicCompat
  ;Save the state we overwrite
  LDX #4
-
  LDA Temp,X
  PHA
  DEX
  BPL -
.ENDIF
  ;Make sure our memory/IO is actually there
  LDA $01
  STA Temp+4
  AND #$F8
  ORA #%00000101
  STA $01
  ;Play the channels
  LDX #(channel.1 - SoundState)
  JSR PlayChannel
  LDX #(channel.2 - SoundState)
  JSR PlayChannel
  LDX #(channel.3 - SoundState)
  JSR PlayChannel
  LDX #(channel.4 - SoundState)
  JSR PlayChannel
  ;Set memory to what it was
  LDA Temp+4
  STA $01
.IFDEF BasicCompat
  LDX #0
-
  PLA
  STA Temp,X
  INX
  CPX #5
  BNE -
.ENDIF
  RTS

PlayChannel:
;Check for Sweep, Virbrato, Tremolo, Percussion Mode
  LDA channel.1.system,X
    PHA
    AND #1
    BEQ +
    JSR _doSweep
+
    PLA
    PHA
    AND #2
    BEQ +
    JSR _doWobble
+
    PLA
  AND #%01000000
  BEQ +
  JSR _doPerc
+
;The SID suddenly shuts up if you flash the gate too fast.
;So, we turn it off a few ticks prior to when the next note hits, if we're supposed to.
;Is the note over?
  DEC channel.1.remain,X
  BEQ +
  LDA channel.1.remain,X
  CMP #$02
  BNE ++
  ;Is the next directive a tie?
  LDA channel.1.now,X
  STA Temp+1
  LDA.w channel.1.now+1,X
  STA Temp+2
  LDY #$00
  LDA (Temp+1),Y
  BNE ++
  ;Note goes off now
  LDA channel.1.control,X
  TAY
  JSR RAMtoIO
  TYA
  STA SIDch.1.Control,X
  JMP IOtoRAM
++
  RTS
+
_nextCommand:
  ;Note over; read new commands
  JSR _nextByte
  TAY
  ;What are we being asked to do?
  AND #$F0
  BEQ +
  SEC
  SBC #$10
  BNE ++
  ;1: Length Change
  ;Get the length
  JSR _nextByte
;Change the targeted length
    PHA
    TYA
    AND #$0F
    STA Temp
    ASL Temp
    ASL Temp
    JSR RAMtoLoops
    TXA
    CLC
    ADC Temp
    TAY
    PLA
  STA loops.1.notelen,Y
;Gear up for next command
  JSR LoopstoRAM
  JMP _nextCommand
++
  SBC #$10
  BNE ++
  ;2: Loop
  JSR _doLoop
  JMP _nextCommand
++
  ;3-15: Rest/Note
  TYA
    PHA
    ;Tying?
    LDA channel.1.system,X
    BMI ++
    JSR _noTie
++
    ;Turn off tying now.
    LDA channel.1.system,X
    AND #$7F
    STA channel.1.system,X
    PLA
    PHA
    AND #$F0  ;Check for rest (don't play)
    SEC
    SBC #$30
    BEQ ++
    SBC #$10
    LSR A
    LSR A
    LSR A
    JSR _playNote
++
    PLA
  ;Update length
  AND #$0F
  STA Temp
  ASL Temp
  ASL Temp
  TXA
  TAY
  JSR RAMtoLoops
  CLC
  ADC Temp
  TAX
  LDA loops.1.notelen,X
  STA channel.1.remain,Y
  RTS
+
;0: Other commands
  TYA
  CMP #$08
  BMI +
  ;Octave
  AND #$07
  STA channel.1.octave,X
  JMP _nextCommand
+
  DEY
  BPL +
  ;Tie/Filter mode
  LDA channel.1.system,X
  ORA #%10000000
  STA channel.1.system,X
  JMP _nextCommand
+
  BNE +
  ;Sweep
  JSR _nextByte
  BEQ ++
  ;Sweep on
  STA channel.1.sweep,X
  LDA channel.1.system,X
  ORA #%00000001
  STA channel.1.system,X
  JMP _nextCommand
++  ;Sweep off
  LDA channel.1.system,X
  AND #%11111110
  STA channel.1.system,X
  JMP _nextCommand
+
  DEY
  BNE +
  ;Virbrato
  JSR _nextByte
  BEQ ++
  ;Virbrato on
  STA channel.1.wobble,X
  AND #$0F
  STA channel.1.wobtime,X
  LDA channel.1.system,X
  ORA #%00000110
  STA channel.1.system,X
  JMP _nextCommand
++  ;Virbrato off
  LDA channel.1.system,X
  AND #%11111001
  STA channel.1.system,X
  JMP _nextCommand
+
  DEY
  BNE +
  ;Tempo
  TXA
  TAY
  JSR _nextByte
  JSR SetTempo
  TYA
  TAX
  JMP _nextCommand
+
  DEY
  BNE +
  ;Control/Filter Enables
  JMP _controlFilter
+
  DEY
  BNE +
  ;ADSR/Volume
  TXA
  BNE ++
  ;Channel 0
  JSR _nextByte
  TAY
  ASL A
  ASL A
  ASL A
  ASL A
  STA.w channel.1.phase+1
  TYA
  ORA channel.1.phase
  STA SIDflt.ModeVol
  JMP _nextCommand
++
  JSR _nextByte
  STA channel.1.adsr,X
  TAY
  JSR _nextByte
  STA.w channel.1.adsr+1,X
    PHA
    JSR RAMtoIO
    PLA
  STA.w SIDch.1.ADSR+1,X
  TYA
  STA SIDch.1.ADSR,X
  JSR IOtoRAM
  JMP _nextCommand
+
  ;Waveform and Phase
  DEY
  BEQ +
  TXA
  BNE ++
  ;Channel 0
  JSR _nextByte
  TAY
  ;Filter type first
  LDA channel.1.phase
  AND #%10000000
  STA channel.1.phase
  TYA
  AND #%01110000
  ORA channel.1.phase
  STA channel.1.phase
  STA SIDflt.ModeVol
  ;Resonance
  LDA channel.1.adsr
  AND #$0F
  STA channel.1.adsr
  TYA
  AND #$0F
  ASL A
  ASL A
  ASL A
  ASL A
  ORA channel.1.adsr
  STA channel.1.adsr
  STA SIDflt.ResoEnable
  JMP _nextCommand
++
  ;Flip the Test bit
  LDA channel.1.control,X
  TAY
  JSR RAMtoIO
  TYA
  ORA #%00001000
  STA SIDch.1.Control,X
  JSR IOtoRAM
+
  ;Get the phase & wave enables
  JSR _nextByte
    PHA
    ;Splice the wave enables into the control register
    AND #$F0
    TAY
    LDA channel.1.control,X
    AND #$0F
    STA channel.1.control,X
    TYA
    ORA channel.1.control,X
    STA channel.1.control,X
    TAY
    ;Get the rest of the duty cycle
    JSR _nextByte
      PHA
      JSR RAMtoIO
      PLA
    STA SIDch.1.PulseWidth,X
    PLA
  STA.w SIDch.1.PulseWidth+1,X
  TYA
  STA SIDch.1.Control,X
  JSR IOtoRAM
  JMP _nextCommand

_controlFilter:
  TXA
  BNE ++
  ;Channel 0
  LDA channel.1.adsr
  AND #$F0
  STA channel.1.adsr
  JSR _nextByte
  ORA channel.1.adsr
  STA channel.1.adsr
  STA SIDflt.ResoEnable
  JMP _nextCommand
++
  ;This one's kinda all over the place
  JSR _nextByte
  TAY
  ;Ring/Sync
  LDA channel.1.control,X
  AND #%11111001
  STA channel.1.control,X
  TYA
  AND #%00000110
  ORA channel.1.control,X
  STA channel.1.control,X
  TYA
  AND #%00001000
  BEQ ++
  ;Channel 3 toggle
  LDA #%10000000
  EOR channel.1.phase
  STA channel.1.phase
  STA SIDflt.ModeVol
++
  ;Percussion Mode
  LDA channel.1.system,X
  AND #%10111111
  STA channel.1.system,X
  TYA
  AND #%00000001
  CLC
  ROR A
  ROR A
  ROR A
  ORA channel.1.system,X
  STA channel.1.system,X
  ;Update SID
  LDY channel.1.control,X
  JSR RAMtoIO
  TYA
  STA SIDch.1.Control,X
  JSR IOtoRAM
  JMP _nextCommand

_noTie:
  ;Gate the note off
  ;Get the shadow control register real quick
  LDA channel.1.control,X
  TAY
  JSR RAMtoIO
  TYA
  AND #%11111110
  STA SIDch.1.Control,X
  JMP IOtoRAM

_playPercussion:
;Get the settings for this note
  TYA
  ASL A
  ASL A
  ASL A
  TAY
  ;Shadows First
  LDA PercData,Y
  STA channel.1.freq,X
  INY
  LDA PercData,Y
  STA.w channel.1.freq+1,X
  INY
  LDA PercData,Y
  STA channel.1.adsr,X
  INY
  LDA PercData,Y
  STA.w channel.1.adsr+1,X
  INY
  LDA PercData,Y
  STA channel.1.phase,X
  INY
  LDA PercData,Y
  STA.w channel.1.phase+1,X
  INY
  LDA PercData,Y
  STA channel.1.control,X
  INY
  LDA PercData,Y
  STA channel.1.octave,X
  DEY
  ;True SID next
  JSR RAMtoIO
  LDA PercData,Y
  DEY
    PHA
    LDA PercData,Y
    STA.w SIDch.1.PulseWidth+1,X
    DEY
    LDA PercData,Y
    STA SIDch.1.PulseWidth,X
    DEY
    LDA PercData,Y
    STA.w SIDch.1.ADSR+1,X
    DEY
    LDA PercData,Y
    STA SIDch.1.ADSR,X
    DEY
    LDA PercData,Y
    STA.w SIDch.1.Frequency+1,X
    DEY
    LDA PercData,Y
    STA SIDch.1.Frequency,X
    PLA
  STA SIDch.1.Control,X
  JMP IOtoRAM

_playNote:
  TAY
  ;Percussion mode?
  LDA channel.1.system,X
  AND #%01000000
  BNE _playPercussion
  ;Nab the shadow control
  LDA channel.1.control,X
  STA Temp
  ;Get the frequency for this note
  LDA (NoteTable),Y
  STA Temp+1
  INC NoteTable
  LDA (NoteTable),Y
  STA Temp+2
  DEC NoteTable
  ;Consider the octave
  LDA #7
  SEC
  SBC channel.1.octave,X
  BEQ +
  ;Shift the note down to the correct octave
  TAY
-
  LSR Temp+2
  ROR Temp+1
  DEY
  BNE -
+
  ;Play the note
  JSR RAMtoIO
  LDA Temp+1
  STA SIDch.1.Frequency,X
  LDA Temp+2
  STA.w SIDch.1.Frequency+1,X
  ;Gate on
  LDA Temp
  ORA #%00000001
  STA SIDch.1.Control,X
  JSR IOtoRAM
  ;Update the shadows
  LDA Temp+1
  STA channel.1.freq,X
  LDA Temp+2
  STA.w channel.1.freq+1,X
-
  RTS

_doPerc:
;Cut the note off if it's over time
  LDA channel.1.octave,X
  BEQ -
  DEC channel.1.octave,X
  BNE -
  ;Time to cut it
  ;Mind the other bits
  LDA channel.1.control,X
  TAY
  JSR RAMtoIO
  TYA
  AND #$FE
  STA SIDch.1.Control,X
  JMP IOtoRAM

_doWobble:
  LDA channel.1.wobble,X
  AND #$F0
  LSR A
  LSR A
  LSR A
  LSR A
  TAY
  LDA channel.1.system,X
  AND #%00000100
  BEQ +
  ;Going down
  TYA
  EOR #$FF
  TAY
  INY
+
  ;This part's channel dependent
  TXA
  BEQ +
  ;Actual SID channel, affect frequency
  TYA
  CLC
  ADC channel.1.freq,X
  STA channel.1.freq,X
  PHA
    TYA
    BMI ++
    ;Positive
    LDA #$00
    BEQ +++
++
    ;Negative
    LDA #$FF
+++
    ADC.w channel.1.freq+1,X
    STA.w channel.1.freq+1,X
    TAY
    JSR RAMtoIO
    TYA
    STA.w SIDch.1.Frequency+1,X
    PLA
  STA SIDch.1.Frequency,X
  JSR IOtoRAM
  JMP ++
+ ;Channel 0, affect volume
  TYA
  CLC
  ADC.w channel.1.phase+1
  STA channel.1.phase+1
  LSR A
  LSR A
  LSR A
  LSR A
  ORA channel.1.phase
  STA SIDflt.ModeVol
++
;Adjust the timings
  DEC channel.1.wobtime,X
  BPL +
  ;Switching time!
  LDA channel.1.system,X
  EOR #%00000100
  STA channel.1.system,X
  LDA channel.1.wobble,X
  AND #$0F
  STA channel.1.wobtime,X
+
  RTS

_doSweep:
  LDA channel.1.sweep,X
    PHP
    CLC
    ADC channel.1.freq,X
    STA channel.1.freq,X
    PLP
  BMI +
  ;True addition
  LDA #$00
  BEQ ++
+
  ;Adding a negative
  LDA #$FF
++
  TAY
  ADC.w channel.1.freq+1,X
  STA.w channel.1.freq+1,X
  ;Over/underflow?
  BMI ++
  BNE +
  ;Can only overflow if adding
  BCC +
  TYA
  BNE +
  ;Overflow
  LDA #$FF
  BNE +++
++
  ;Underflow?
  BCS +
  ;Can only underflow if we were subtracting
  TYA
  BPL +
  LDA #$00
+++
  STA channel.1.freq,X
  STA.w channel.1.freq+1,X
+
  TXA
  BNE +
  ;Channel 0, affect filter freq
  LDA channel.1.freq+1
  ;Different caps
  CMP #$08
  BMI ++
  LDA #$07
  STA channel.1.freq+1
  LDA #$FF
  STA channel.1.freq
++
  LDA channel.1.freq+1
  AND #$07
  STA channel.1.freq+1
  ;Transfer the filter frequency
  LDA channel.1.freq
  STA SIDflt.Frequency
  AND #$F8
  ASL A
  ORA.w channel.1.freq+1
  ROR A
  ROR A
  ROR A
  ROR A
  STA.w SIDflt.Frequency+1
  RTS
+ ;Actual SID channel, affect freq
  LDA channel.1.freq,X
  LDY.w channel.1.freq+1,X
    PHA
    JSR RAMtoIO
    PLA
  STA SIDch.1.Frequency,X
  TYA
  STA.w SIDch.1.Frequency+1,X
  JMP IOtoRAM

_doLoop:
  ;Y to the loop ram address
  TYA
  AND #$0F
  STA Temp
  ASL Temp
  ASL Temp
  TXA
    PHA
    JSR RAMtoLoops
    CLC
    ADC Temp
    TAY
    PLA
  TAX
  ;Start or end of loop?
  JSR _nextByte
  ADC #$00      ;Set flags according to A
  BMI +
  ;Set loop
  SBC #$01  ;Account for this directive
  CLC
  ADC channel.1.now,X
  STA loops.1.loopptr,Y
  LDA #$00
  ADC.w channel.1.now+1,X
  STA.w loops.1.loopptr+1,Y
  RTS
+ ;Goto
  AND #$7F
  BEQ +
  ;End of loop?
    PHA
    TXA
    STY Temp
    LDX Temp
    DEC loops.1.loopcnt,X
    BMI ++
    BNE +++
    ;Do not follow
    TAX
    PLA
  RTS
++
    ;First time
    TAX
    PLA
  STA loops.1.loopcnt,Y
  BNE +
+++
    ;Stack align and follow
    TAX
    PLA
+
  ;Follow loop
  LDA loops.1.loopptr,Y
  STA channel.1.now,X
  LDA.w loops.1.loopptr+1,Y
  STA.w channel.1.now+1,X
  RTS

_nextByte:
  LDA channel.1.now,X
  STA Temp+1
  LDA.w channel.1.now+1,X
  STA Temp+2
  STX Temp+3
  LDX #$00
  LDA (Temp+1,X)
  LDX Temp+3
  INC channel.1.now,X
  BNE +
  INC.w channel.1.now+1,X
+
  RTS

  ;Convert X from memory index to a SID index for this channel
  ;Music Channel memory use is 14 bytes, and one SID channel is 7 bytes
  ;Channel 0 gets a dummy IO address
  ;All this stuff is hardcoded since the code has to change if the addresses do
RAMtoIO:
  TXA
  LSR A
  LSR A
  LSR A
  TAX
  LDA.w IOLUT,X
  TAX
  RTS
IOLUT:
;     0   1   -   2   -   3
 .db $19,$00,$00,$07,$00,$0E

RAMtoLoops:
  TXA
  LSR A
  LSR A
  LSR A
  TAX
  LDA.w LoopLUT,X
  TAX
  RTS

LoopstoRAM:
  TXA
  CLC
  ROL A
  ROL A
  ROL A
  TAX
  LDA.w RAMLUT,X
  TAX
  RTS
RAMLUT:
;Lp:  0   1   2   3
 .db $00
    ;IO:  1   2   -   3   -   -   0
     .db $0F,$1E,$2D,$2D
LoopLUT:
                    ;     0   1   -   2   -   3
                     .db $00,$40,$00,$80,$00,$C0

IOtoRAM:
  TXA
  LSR A
  LSR A
  TAX
  LDA.w RAMLUT+1,X
  TAX
  RTS

;A=New Tempo
;Destroys A,X
SetTempo:
;Get predefined machine type
  LDX Clock
  STX DIVAREA
  LDX.w Clock+1
  STX DIVAREA+1
  LDX #$0E
  STX DIVAREA+2
  JSR TempoDiv
;Set the timer
;Assumedly, since we're running commands, we want the timer to be on
  LDA DIVAREA+3
  STA.w CIA2.TimerB
  LDA DIVAREA+4
  STA.w CIA2.TimerB+1
  LDA #%00000001
  STA CIA2.TimControlB
  LDA #%00011101
  STA CIA2.IntControl
  LDA #%10000010
  STA CIA2.IntControl
  RTS

.ENDS

.SECTION "Sound Utils" FREE

;Div function to convert tempo to timer values
;Requires 5 bytes zp space
;Place 3-byte Divdend at Divarea
;Place 1-byte Divisor in A
;2-byte Quotient will be available at Divarea+3
;Takes about 600 cycles, depending on the divisor
TempoDiv:
  LDX #$01
  CMP #16
  BCS +
;Divisor too tiny; bias the result so it fits in 16 bits
  ASL A
  ASL A
  ASL A
  ASL A
  LDX #16
+
  STX.w SlowFlag
  STX.w Delay
  TAX
  LDA DIVAREA+2
  STX DIVAREA+2
  LDX #16
-       ;Central division loop
  ASL DIVAREA
  ROL DIVAREA+1
  ROL A
  BCS +
  CMP DIVAREA+2
  BCC ++
;Bit set
  SEC
+
  SBC DIVAREA+2
  SEC
++      ;If we jumped here, bit is clear
;Carry is set appropriately
  ROL DIVAREA+3
  ROL DIVAREA+4
  DEX
  BNE -
  RTS

.ENDS
