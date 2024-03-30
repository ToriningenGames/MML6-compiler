.INCLUDE IO.inc

;RAM use
.DEFINE DIVAREA         $10     ;5 bytes ZP
.DEFINE NoteTable       $15     ;2 bytes ZP
.DEFINE SoundState      $2000   ;2 pages, page aligned

.STRUCT LoopLength
length  db
loopcnt db
loopptr dw
.ENDST

.STRUCT MusicChannel
system  db
freq    dw
phase   dw
control db
adsr    dw
now     dw
remain  db
octave  db
.ENDST

.ENUM SoundState
channel         INSTANCEOF MusicChannel 4
SlowFlag        db
Clock           dw
.ENDE
.ENUM SoundState + $0100
loops   INSTANCEOF LoopLength 4*16
.ENDE

;Useful Constants
.DEFINE NtscTimer       $0EA157
.DEFINE PalTimer        $0E1816
.DEFINE DreanTimer      $0EA3F7

.SECTION "Freq Table" FREE ALIGN 2
;     A     A+    B     C     C+    D     D+    E     F     F+    G     G+
NtscFreq:
 .dw $70C8,$777D,$7E97,$861E,$8E18,$968B,$9F7F,$A8FB,$B307,$BDAC,$C8F4,$D4E7
PalFreq:
 .dw $7512,$7C08,$8368,$8B39,$9380,$9C45,$A590,$AF68,$B9D6,$C4E3,$D099,$DD00
DreanFreq:
 .dw $70B3,$7767,$7E81,$8606,$8DFE,$9670,$9F62,$A8DC,$B2E7,$BD8A,$C8D0,$D4C0
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
  ADC $00
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
  ;...
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

NewSong:
PlayTick:
  ;Note: Use the ZP note table addr with indirect-indexed with the note in Y
  ;This gets the frequency for this note
  LDX #(channel.0 - SoundState)
  JSR PlayChannel
  LDX #(channel.1 - SoundState)
  JSR PlayChannel
  LDX #(channel.2 - SoundState)
  JSR PlayChannel
  LDX #(channel.3 - SoundState)
  ;Fall through
PlayChannel:
SetTempo:
;Get predefined machine type

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
  STX SlowFlag
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
