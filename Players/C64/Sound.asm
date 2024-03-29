;RAM use
.DEFINE SlowFlag        $06     ;One byte
.DEFINE Clock           $00     ;Three bytes
.DEFINE DIVAREA         $10     ;Five bytes ZP

;Useful Constants
.DEFINE NtscTimer       $0EA157
.DEFINE PalTimer        $0E1816
.DEFINE DreanTimer      $0EA3F7

.SECTION "Freq Table" FREE
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
  CMP #$7F
  BEQ Pal
  CMP #$32
  BEQ Pal
  CMP #$70
  BEQ Drean
  CMP #$20
  BEQ Drean
Ntsc:
  LDA #(NtscTimer & $FF)
  LDX #((NtscTimer & $FF00) >> 8)
  LDY #((NtscTimer & $FF0000) >> 16)
  BNE SysCommon
Pal:
  LDA #(PalTimer & $FF)
  LDX #((PalTimer & $FF00) >> 8)
  LDY #((PalTimer & $FF0000) >> 16)
  BNE SysCommon
Drean:
  LDA #(DreanTimer & $FF)
  LDX #((DreanTimer & $FF00) >> 8)
  LDY #((DreanTimer & $FF0000) >> 16)
SysCommon:
  STA Clock
  STX Clock+1
  STY Clock+2
;Set sane ram defaults
  ;...
;Hook NMI. We assume nothing important was there.
.IFDEF KernalCompat
;Hijack the NMI from the Kernal
  STA $0318
  STA $0319
.ELSE
;Not using the Kernal; patch NMI directly
  STA $FFFA
  STA $FFFB
.ENDIF
  ;...
  RTS

.ENDS

.SECTION "Sound Functions" FREE

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
