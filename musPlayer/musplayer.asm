;Music player
;Plays compiled MML files.
;Intended for public consumption

.MEMORYMAP
SLOTSIZE $4000
DEFAULTSLOT 0
SLOT 0 $0000
SLOT 1 $4000
.ENDME

.ROMBANKMAP
BANKSTOTAL 2
BANKSIZE $4000
BANKS 2
.ENDRO

.EMPTYFILL $FF

.BANK 0 SLOT 0
.ORG $00
;RST $00
  RET
.ORG $08
;RST $08
  RET
.ORG $10
;RST $10
  RET
.ORG $18
;RST $18
  RET
.ORG $20
;RST $20
  RET
.ORG $28
;RST $28
  RET
.ORG $30
;RST $30
  JP HL
.ORG $38
;RST $38
  DI
  HALT
  HALT
.ORG $40
;vBlank
  PUSH HL
  PUSH DE
  PUSH BC
  PUSH AF
  JP vBlank
.ORG $48
;LCD
  RETI
.ORG $50
;Timer
  RETI
.ORG $58
;Serial
  RETI
.ORG $60
;Joypad
  RETI

.ORG $0100
.SECTION "Header" SIZE $4D FORCE
;Entry
  DI
  JP Start
;Nintendo Logo (48 bytes)
 .db $CE,$ED,$66,$66,$CC,$0D,$00,$0B,$03,$73,$00,$83,$00,$0C,$00,$0D
 .db $00,$08,$11,$1F,$88,$89,$00,$0E,$DC,$CC,$6E,$E6,$DD,$DD,$D9,$99
 .db $BB,$BB,$67,$63,$6E,$0E,$EC,$CC,$DD,$DC,$99,$9F,$BB,$B9,$33,$3E
;Title (16 bytes on DMG, 11 bytes elsewhere)
 .db "MusPlayer",0,0
;     123456789  A B
;Manufacturer code
 .db $00,$00,$00,$00
;Color Game Boy flag
 .db $00
;New Licensee Code
 .db $00,$00
;Super Game Boy flag
 .db $00
;Cartridge type
 .db $00
;ROM size
 .db $00
;RAM size
 .db $00
;Release destination
 .db $01            ;Not Japan
;Old Licensee code
 .db $CD            ;Made-up license
;Mask ROM version
 .db $00

.COMPUTEGBCHECKSUM
.COMPUTEGBCOMPLEMENTCHECK

.ENDS

.SECTION "Init" FREE
Start:
;Dim screen
  LD A,%00010000
  LDH ($41),A
  XOR A
  LDH ($0F),A   ;We want to know when vBlank is
  ;vBlank assumes we play music, so we use LCD IRQ for vBlank detection
  LD A,%00000010
  LDH ($FF),A
  EI
  LD C,15
-
  HALT
  DEC C
  JR nz,-
  LD A,%01011101
  LDH ($47),A
  LD C,15
-
  HALT
  DEC C
  JR nz,-
  LD A,%10101110
  LDH ($47),A
  LD C,15
-
  HALT
  DEC C
  JR nz,-
  LD A,%11111111
  LDH ($47),A
;Load graphics
  LD HL,Tiledata
  LD DE,$8000
  LD B,20       ;1888 bytes over 20 frames
-
  LD C,96
  HALT
--
  LDI A,(HL)
  LD (DE),A
  INC DE
  DEC C
  JR nz,--
  DEC B
  JR nz,-
;Tilemap
  LD HL,MAP001MUSDEMO
  LD DE,$9800
  LD B,7        ;576 bytes over 6 frames
-
  LD C,96
  HALT
--
  LDI A,(HL)
  LD (DE),A
  INC DE
  DEC C
  JR nz,--
  DEC B
  JR nz,-
;OAM
  CALL OAMInit
;Managerial things
  LD A,%10010011
  LDH ($40),A
  ;Tiles
  XOR A
  LD HL,$C000
  LD BC,$0120+$0100
-
  LDI (HL),A
  DEC C
  JR nz,-
  DEC B
  JR nz,-
  ;Sprites, draw areas
  LD A,$30
  LD ($C100),A  ;Channel 1 note Y
  LD A,$50
  LD ($C104),A  ;Channel 2 note Y
  LD A,$70
  LD ($C108),A  ;Channel 3 note Y
  LD A,$90
  LD ($C10C),A  ;Channel 4 note Y
  LD A,$10
  LD ($C110),A  ;Master pan left outer sprite Y
  LD ($C114),A  ;Master pan left inner sprite Y
  LD ($C118),A  ;Master pan right outer sprite Y
  LD ($C11C),A  ;Master pan right inner sprite Y
  LD A,$73
  LD ($C112),A  ;Master pan left outer sprite tile
  LD ($C11A),A  ;Master pan right outer sprite tile
  LD A,$72
  LD ($C116),A  ;Master pan left inner sprite tile
  LD ($C11E),A  ;Master pan right inner sprite tile
  LD A,%00100000
  LD ($C117),A  ;Master pan left inner sprite attribute
  
  LD A,%00100111
  LDH ($48),A   ;Palette
  
  LD A,$00
  LD ($C00E),A
  LD A,$82
  LD ($C00F),A
  LD A,$00
  LD ($C010),A
  LD A,$D0
  LD ($C011),A
;Undim graphics
  LD C,15
-
  HALT
  DEC C
  JR nz,-
  LD A,%11111110
  LDH ($47),A
  LD C,15
-
  HALT
  DEC C
  JR nz,-
  LD A,%11101001
  LDH ($47),A
  LD C,15
-
  HALT
  DEC C
  JR nz,-
  LD A,%11100100
  LDH ($47),A
;Load music
;Initialize sound section
  LD HL,musicglobalbase
  XOR A
  LD B,A
-
  LDI (HL),A    ;Set all sound section to 0
  DEC B
  JR nz,-
  LD HL,channelonebase+$2A
  LD BC,Channel1Pitch
  LD (HL),C
  INC L
  LD (HL),B
  LD L,<channeltwobase+$2A
  LD BC,Channel2Pitch
  LD (HL),C
  INC L
  LD (HL),B
  LD L,<channelthreebase+$2A
  LD BC,Channel3Pitch
  LD (HL),C
  INC L
  LD (HL),B
  LD L,<channelfourbase+$2A
  LD BC,Channel4Pitch
  LD (HL),C
  INC L
  LD (HL),B

;Song Load
  LD BC,Song
  CALL MusicLoad
  LD A,$FF
  LD (musicglobalbase+1),A
  LD A,$FF
  LDH ($26),A
  LDH ($24),A
  LDH ($25),A
;Wait for Start
  LD C,0
  LD A,%00010000
-
  HALT
  LDH (C),A
  LDH A,(C)
  LDH A,(C)
  LDH A,(C)
  AND %00001000
  JR nz,-
;Go!
  XOR A
  LDH ($0F),A   ;Switch interrupt
  INC A
  LDH ($FF),A
;Follow song as needed
SongLoop:
  HALT
;Set global register values (things that may change from a managing program)
;Global pan
  LDH A,($24)
  LD B,A
  LD HL,LoutLUT
  AND %01110000
  SWAP A
  ADD L
  LD L,A
  LD A,(HL)
  LD ($C111),A  ;Master pan left outer sprite X
  LD A,8
  ADD L
  LD L,A
  LD A,(HL)
  LD ($C115),A  ;Master pan left inner sprite X
  LD A,%00000111
  AND B
  ADD <RoutLUT
  LD L,A
  LD A,(HL)
  LD ($C119),A  ;Master pan right outer sprite X
  LD A,8
  ADD L
  LD L,A
  LD A,(HL)
  LD ($C11D),A  ;Master pan right inner sprite X
  
  LDH A,($25)
  LD HL,$C000
  LD B,A
  XOR A
;Channel 1 pan
  LD (HL),A
  BIT 4,B
  JR z,+
  LD (HL),$10
+
  INC L
  LD (HL),A
  BIT 0,B
  JR z,+
  LD (HL),$13
+
  INC L
;Channel 2 pan
  LD (HL),A
  BIT 5,B
  JR z,+
  LD (HL),$10
+
  INC L
  LD (HL),A
  BIT 1,B
  JR z,+
  LD (HL),$13
+
  INC L
;Channel 3 pan
  LD (HL),A
  BIT 6,B
  JR z,+
  LD (HL),$10
+
  INC L
  LD (HL),A
  BIT 2,B
  JR z,+
  LD (HL),$13
+
  INC L
;Channel 4 pan
  LD (HL),A
  BIT 7,B
  JR z,+
  LD (HL),$10
+
  INC L
  LD (HL),A
  BIT 3,B
  JR z,+
  LD (HL),$13
+
;For each channel
    ;Read old channel state from $C200
    ;Compare with live state
;Channel 1
  LD HL,$C210
  LDH A,($12)   ;current Envelope
  CP (HL)
  JR nz,+       ;Env changed
  INC L
  LD A,(channelonebase+$31)     ;current Stacatto
  CP (HL)
  JP z,++       ;Stacatto didn't change
  DEC L
+       ;New waveform
  LDH A,($12)   ;New Envelope
  LDI (HL),A
  LD A,(channelonebase+$31)     ;New Stacatto
  LD (HL),A
  LD B,A
  LD HL,$C400
  LD A,$07
  LD C,64
  AND B
  LD E,A
  LD D,$FF
  BIT 3,B
  JR z,+
  LD D,1
+
  LD A,$07
  AND B ;Check for zero step
  JR nz,+
  LD D,0
+
  LD A,$F0
  AND B
  SWAP A
    PUSH DE
-
    LDI (HL),A
    DEC E
    JR nz,+
    POP DE
    PUSH DE
    ADD D
    BIT 4,A   ;Overflow checks
    JR z,+
    SUB D
+
    DEC C
    JR nz,-
    POP DE
  ;Get stacatto point
  LD A,(channelonebase+$31)  ;CH1 Stacatto
  OR A  ;Don't draw if disabled
  JR z,+
  ;Dot equation: 16-A/4
  SRA A ;Divide by 4
  SRA A
  CPL       ;Negate; round to nearest (slightly incorrect due to DEC earlier)
  ADC 18    ;Rounding + other half of negate + 16 + 1 for decrement following
+
  DEC A
  LD B,A
  LD HL,$D000
  LD DE,$C400
-
;Given Tile Data pointer and Envelope Data pointer are correct,
;This will set carry according to whether or not a dot gets drawn here
  LD A,%00001110
  AND L
  RRCA
  BIT 7,L
  JR z,+
  SET 3,A
+
  CPL
  AND $0F   ;0-15 -> (16-1) minus 1
  LD C,A
  LDH A,($12)
  CP C      ;Carry clear if pixel gets drawn here
  CCF
;Clears carry if stacatto says so.
  JR nc,+
  BIT 0,L
  JR z,+
  LD A,E
  CP B
+
  RL (HL)
  INC L
  LD A,$0F
  AND L
  JR nz,-
  LD A,L    ;Set E to be consistent with L
  RRCA
  AND %00111000
  LD C,A
  LD A,$07
  AND E
  OR C
  LD E,A
  LD A,L    ;Test if L overflowed,
  OR A
  JR nz,-
  INC E     ;increase other dimension if so.
  LD A,$07  ;If the dimension part of E overflowed,
  AND E     ;finish.
  JR nz,-
  
  ;Overwrite with stacatto line
  LD A,B    ;Draw the dotted line here
  CP 64     ;Don't draw if disabled
  JR nc,+++++
  LD H,$D2
  AND $78
  RLA
  LD L,A
  LD A,$07
  AND B
  LD B,A
  ;B = pixel offset
  ;HL-> correct tile
  LD E,2
---
  LD D,8
--
  LD A,E
  OR A
  JR nz,+++ ;This makes the line dotted
  INC L
  INC L
  LD E,2
  DEC D
  JR z,++++
+++
  LD C,B
  LD A,(HL)
-   ;Lo bit
  RLCA
  DEC C
  JR nz,-
  OR A  ;Clear Carry
  RRA
  RLCA
  LD C,B
-
  RRCA
  DEC C
  JR nz,-
  LDI (HL),A
  LD C,B
  LD A,(HL)
-   ;Hi bit
  RLCA
  DEC C
  JR nz,-
  SCF
  RRA
  RLCA
  LD C,B
-
  RRCA
  DEC C
  JR nz,-
  LDI (HL),A
  DEC E
  DEC D
  JR nz,--
++++
  BIT 7,L
  JR nz,+++++
  LD A,$70  ;Go to low tiles
  ADD L
  LD L,A
  JR ---

+++++
++
  LD HL,$C213   ;Old wave
  LDH A,($11)   ;Curr Wave
  CP (HL)
  JR z,+
;New wave
  LD (HL),A
  LD BC,$C008
  AND $C0
  SWAP A
  RRCA
  ADD $64
  LD (BC),A
  INC C
  INC A
  LD (BC),A
+
  INC L
  LDH A,($81)   ;New note
  CP (HL)       ;Old note
  JR z,+
;New note
  LD (HL),A
  ;Get octave
  LD A,(channelonebase+$2C)
  DEC A
  LD C,A        ;Go to half steps from base
  ADD A   ;*2
  ADD C   ;*3
  ADD A   ;*6
  ADD A   ;*12
  LD C,A
  LD A,(HL)
  SWAP A
  AND $0F
  SUB $04
;"Note" is half steps from table base
;Or $F9 for a rest (auto-pushes to offscreen)
;Position: (N) * 2 + 14
;Sprite: LUT + N % 12
  ADD C
  RLCA
  ADD 8
  LD ($C101),A
  LD BC,NoteLUT
  LD A,(HL)
  SWAP A
  AND $0F
  SUB $04
-
  SUB 12
  JR nc,-
  ADD 12
  ADD C
  LD C,A
  LD A,(BC)
  LD ($C102),A
;Adjust priority for white notes
;No, not like that!
  RRCA
  CPL
  AND $80
  LD ($C103),A
+
  INC E
  INC L
;Channel 2
  LD HL,$C220
  LDH A,($17)   ;current Envelope
  CP (HL)
  JR nz,+       ;Env changed
  INC L
  LD A,(channeltwobase+$31)     ;current Stacatto
  CP (HL)
  JP z,++       ;Stacatto didn't change
  DEC L
+       ;New waveform
  ;Write every vertical pixel strips's envelope height, in order
  LDH A,($17)   ;New Envelope
  LDI (HL),A
  LD A,(channeltwobase+$31)     ;New Stacatto
  LD (HL),A
  LD B,A
  LD HL,$C500
  LD A,$07
  LD C,64
  AND B
  LD E,A
  LD D,$FF
  BIT 3,B
  JR z,+
  LD D,1
+
  LD A,$07
  AND B ;Check for zero step
  JR nz,+
  LD D,0
+
  LD A,$F0
  AND B
  SWAP A
  PUSH DE
  ;Write volume
  ;affect step counter
    ;if counter 0
      ;reload counter
      ;apply step
  ;dec c
    ;exit if C==0
  ;Variables:
    ;Current volume (A)
    ;Step direction (D)
    ;Counter        (E)
    ;Counter init   (stack)
    ;C
-
  LDI (HL),A
  DEC E
  JR nz,+
  POP DE
  PUSH DE
  ADD D
  BIT 4,A   ;Overflow checks
  JR z,+
  SUB D
+
  DEC C
  JR nz,-
  POP DE
  ;Get stacatto point
  LD A,(channeltwobase+$31)  ;CH2 Stacatto
  OR A  ;Don't draw if disabled
  JR z,+
  ;Dot equation: 16-A/4
  SRA A ;Divide by 4
  SRA A
  CPL       ;Negate; round to nearest (slightly incorrect due to DEC earlier)
  ADC 18    ;Rounding + other half of negate + 16 + 1 for decrement following
+
  DEC A
  LD B,A
  LD HL,$D100
  LD DE,$C500
  ;Draw envelope profile
  ;Put out low bit
  ;Check if at/past stacatto
    ;If so, clear carry
  ;Put out high bit
  ;Variables:
    ;Leftward counter   (E) (from left)
    ;Downward counter   (L %3---210-) (from top)
    ;Stacatto point     (B) (preserve!)
  ;Pointers:
    ;Tile data (HL)
;Order for bits?
  ;Please keep HL consistent; it's a pain to move around
    ;HL increments
    ;DE:
      ;01234567 x16
      ;89ABCDEF x16
-
;Given Tile Data pointer and Envelope Data pointer are correct,
;This will set carry according to whether or not a dot gets drawn here
  LD A,%00001110
  AND L
  RRCA
  BIT 7,L
  JR z,+
  SET 3,A
+
  CPL
  AND $0F   ;0-15 -> (16-1) minus 1
  LD C,A
  LDH A,($17)
  CP C      ;Carry clear if pixel gets drawn here
  CCF
;Clears carry if stacatto says so.
  JR nc,+
  BIT 0,L
  JR z,+
  LD A,E
  CP B
+
  RL (HL)
  INC L
  LD A,$0F
  AND L
  JR nz,-
  LD A,L    ;Set E to be consistent with L
  RRCA
  AND %00111000
  LD C,A
  LD A,$07
  AND E
  OR C
  LD E,A
  LD A,L    ;Test if L overflowed,
  OR A
  JR nz,-
  INC E     ;increase other dimension if so.
  LD A,$07  ;If the dimension part of E overflowed,
  AND E     ;finish.
  JR nz,-
  
  ;Overwrite with stacatto line
  LD A,B    ;Draw the dotted line here
  CP 64     ;Don't draw if disabled
  JR nc,+++++
  LD H,$D1
  AND $78
  RLA
  LD L,A
  LD A,$07
  AND B
  LD B,A
  ;B = pixel offset
  ;HL-> correct tile
  LD E,2
---
  LD D,8
--
  LD A,E
  OR A
  JR nz,+++ ;This makes the line dotted
  INC L
  INC L
  LD E,2
  DEC D
  JR z,++++
+++
  LD C,B
  LD A,(HL)
-   ;Lo bit
  RLCA
  DEC C
  JR nz,-
  OR A  ;Clear Carry
  RRA
  RLCA
  LD C,B
-
  RRCA
  DEC C
  JR nz,-
  LDI (HL),A
  LD C,B
  LD A,(HL)
-   ;Hi bit
  RLCA
  DEC C
  JR nz,-
  SCF
  RRA
  RLCA
  LD C,B
-
  RRCA
  DEC C
  JR nz,-
  LDI (HL),A
  DEC E
  DEC D
  JR nz,--
++++
  BIT 7,L
  JR nz,+++++
  LD A,$70  ;Go to low tiles
  ADD L
  LD L,A
  JR ---

+++++
++
  LD HL,$C223   ;Old wave
  LDH A,($16)   ;Curr Wave
  CP (HL)
  JR z,+
;New wave
  LD (HL),A
  LD BC,$C00A
  AND $C0
  SWAP A
  RRCA
  ADD $64
  LD (BC),A
  INC C
  INC A
  LD (BC),A
+
  INC L
  LDH A,($82)   ;New note
  CP (HL)       ;Old note
  JR z,+
;New note
  LD (HL),A
  ;Get octave
  LD A,(channeltwobase+$2C)
  DEC A
  LD C,A        ;Go to half steps from base
  ADD A   ;*2
  ADD C   ;*3
  ADD A   ;*6
  ADD A   ;*12
  LD C,A
  LD A,(HL)
  SWAP A
  AND $0F
  SUB $04
  ADD C
  ;Transform to x pos
  RLCA
  ADD 8
  LD ($C105),A
  LD BC,NoteLUT
  LD A,(HL)
  SWAP A
  AND $0F
  SUB $04
-
  SUB 12
  JR nc,-
  ADD 12
  ADD C
  LD C,A
  LD A,(BC)
  LD ($C106),A
  RRCA
  CPL
  AND $80
  LD ($C107),A
+
  INC E
  INC L
;Channel 3
  LD HL,$C230
  LDH A,($1C)     ;Curr Envelope
  CP (HL)   ;Env changed?
  JR z,++
;New envelope
  LD (HL),A
  LD BC,$C00C
  LD A,$74  ;Zero catch
  LD (BC),A
  INC C
  LD (BC),A
  DEC C
  LD A,(HL)
  AND $60
  JR z,++
  LD A,$6C      ;Always at least 25%
  LD (BC),A
  LD A,(HL)     ;75% or 100%?
  AND %01100000
  SWAP A
  RRCA
  ADD $74
  LD ($C00D),A
++
  INC L
  LDH A,($85)     ;Current Wave
  CP (HL)
  JR z,+
;New wave
  LD (HL),A
  LD DE,Wave    ;Go to this wave
  SWAP A
  LD H,A
  AND $F0
  LD L,A
  LD A,$0F
  AND H
  LD H,A
  ADD HL,DE
  PUSH HL
  PUSH HL
  LD E,L
  LD D,H
  LD HL,$D200   ;Go to visual wave data
  LD C,$0F
--
  LD B,C
-
  LD A,(DE)     ;Check if we have dots here
  SWAP A
  AND $0F
  CP B
  SCF
  JR z,++
  ;No dot
  CCF
++
  RL (HL)
  LD A,(DE) ;Both nibbles
  AND $0F
  CP B
  SCF
  JR z,++
  CCF
++
  RL (HL)
  INC E ;For four bytes
  LD A,$03
  AND E
  JR nz,-
  LDI A,(HL)
  LDI (HL),A    ;Byte doubling (2 bytes/line for tiles)
  POP DE    ;Reset wave
  PUSH DE
  DEC B
  LD A,B    ;Only half the range considered
  CPL
  AND $07
  JR nz,-
  POP DE    ;Permamently advance wave
  INC E
  INC E
  INC E
  INC E
  LD A,E
  AND $0F
  JR nz,++
  LD A,E
  SUB $10   ;Stay in wave
  LD E,A
  LD A,D
  SBC 0
  LD D,A
++
  PUSH DE
  LD A,L
  AND %01111111
  JR nz,--
  POP DE
  POP DE
  PUSH DE
  PUSH DE
  LD C,$07  ;Do low half
  BIT 7,L
  JR nz,--
  
  POP DE
  POP DE
+
  LD HL,$C232
  LDH A,($83)     ;Current note
  CP (HL)
  JR z,+
;New note
  LD (HL),A
  LD A,(channelthreebase+$2C)
  LD C,A        ;Go to half steps from base
  ADD A   ;*2
  ADD C   ;*3
  ADD A   ;*6
  ADD A   ;*12
  LD C,A
  LD A,(HL)
  SWAP A
  AND $0F
  SUB $04
  ADD C
  RLCA
  ADD 8
  LD ($C109),A
  LD BC,NoteLUT
  LD A,(HL)
  SWAP A
  AND $0F
  SUB $04
-
  SUB 12
  JR nc,-
  ADD 12
  ADD C
  LD C,A
  LD A,(BC)
  LD ($C10A),A
  RRCA
  CPL
  AND $80
  LD ($C10B),A
+
;Channel 4
  LD HL,$C240
  LDH A,($84)     ;Curr note
  CP (HL)
  JR z,+
;New note
  LD (HL),A
  LD A,(channelfourbase+$2C)
  LD C,A        ;Go to half steps from base
  ADD A   ;*2
  ADD C   ;*3
  ADD A   ;*6
  ADD A   ;*12
  LD C,A
  LD A,(HL)
  SWAP A
  AND $0F
  SUB $04
  ADD C
  RLCA
  ADD 8
  LD ($C10D),A
  LD BC,NoteLUT
  LD A,(HL)
  SWAP A
  AND $0F
  SUB $04
-
  SUB 12
  JR nc,-
  ADD 12
  ADD C
  LD C,A
  LD A,(BC)
  LD ($C10E),A
  RRCA
  CPL
  AND $80
  LD ($C10F),A
+
  JP SongLoop
.ENDS
.SECTION "LUTs" ALIGN 64 FREE
LoutLUT:
 .db $40,$40,$40,$40,$3E,$3C,$3A,$38
LinLUT:
 .db $48,$46,$44,$40,$3E,$3C,$3A,$38
RoutLUT:
 .db $68,$68,$68,$68,$6A,$6C,$6E,$70
RinLUT:
 .db $60,$62,$64,$66,$68,$6A,$6C,$6E
NoteLUT:
;     A   A+  B   C   C+  D   D+  E   F   F+  G   G+
 .db $70,$71,$70,$70,$71,$70,$71,$70,$70,$71,$70,$71
.ENDS

.SECTION "vBlank" FREE
;vBlank
vBlank:
;Graphics
  LD HL,$C11F   ;8 sprites * 4 bytes/sprite
  LD BC,$FE1F
-
  LDD A,(HL)
  LD (BC),A
  DEC C
  JR nz,-
  LDD A,(HL)
  LD (BC),A
  INC L
;Tile choices
  LDI A,(HL)
  LD ($986F),A  ;Channel 1 pan
  LDI A,(HL)
  LD ($9872),A
  LDI A,(HL)
  LD ($98EF),A  ;Channel 2 pan
  LDI A,(HL)
  LD ($98F2),A
  LDI A,(HL)
  LD ($996F),A  ;Channel 3 pan
  LDI A,(HL)
  LD ($9972),A
  LDI A,(HL)
  LD ($99EF),A  ;Channel 4 pan
  LDI A,(HL)
  LD ($99F2),A
  LDI A,(HL)
  LD ($9863),A  ;Channel 1 duty
  LDI A,(HL)
  LD ($9864),A
  LDI A,(HL)
  LD ($98E3),A  ;Channel 2 duty
  LDI A,(HL)
  LD ($98E4),A
  LDI A,(HL)
  LD ($9963),A  ;Channel 3 volume
  LDI A,(HL)
  LD ($9964),A
;Budget: 2956
;Needed Throughput: 1024 bytes
  PUSH HL
  LD E,(HL)
  INC L
  LD D,(HL)
  INC L
  LDI A,(HL)
  LD H,(HL)
  LD L,A
  LD B,64
;Budget: 2888 (<66)
;Therefore, 64 bytes/frame, for 12 frames (no channel 4)
-
  LDI A,(HL)
  LD (DE),A
  INC DE
  DEC B
  JR nz,-
;Critical section finished
  LD A,$85
  CP D
  JR nz,+
  LD HL,$D000
  LD DE,$8200
+
  LD C,H
  LD A,L
  POP HL
  LD (HL),E
  INC L
  LD (HL),D
  INC L
  LDI (HL),A
  LD (HL),C
;Sound
  CALL PlayTick
  POP AF
  POP BC
  POP DE
  POP HL
  RETI
.ENDS

.ORG $0FFC

.SECTION "OAM" FORCE
;This is emulator unfriendly and absolutely evil.
    ;When OAM DMA is started, the bus is stolen to transfer the bytes.
    ;As a side effect, when the processor reads the bus during this time,
    ;it gets the byte currently being transferred.
    ;Since we're setting OAM to all 0s and $00 is a NOP, we can skip
    ;putting a routine in HRAM and just execute the OAM data!
  LD A,>OAMData ;$10
  LDH ($46),A
OAMData:
 .dsb $A0,0
  RET
;For the sake of testing, use this instead
OAMInit:
  LD C,$80
  LD B,8
  LD HL,OAMRoutine
-
  LDI A,(HL)
  LDH (C),A
  INC C
  DEC B
  JR nz,-
  LD A,>OAMData
  JP $FF80
OAMRoutine:
  LDH ($46),A
  LD A,$A0/4
-
  DEC A
  JR nz,-
  RET
.ENDS

.include "playerSongs.asm"

.SECTION "Tile" FREE
Tiledata:
.incbin "piano.tile"
.ENDS

.SECTION "Maps" FREE
;Screen

MAP001MUSDEMO:
 .db $00,$00,$00,$00,$00,$00,$00,$60,$61,$11,$12,$62,$63,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
 .db $00,$00,$00,$00,$00,$0A,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0C,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
 .db $01,$02,$03,$04,$06,$05,$20,$21,$22,$23,$24,$25,$26,$27,$0F,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
 .db $00,$00,$0D,$68,$69,$0E,$28,$29,$2A,$2B,$2C,$2D,$2E,$2F,$0F,$10,$11,$12,$13,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
 .db $1E,$18,$19,$1A,$18,$19,$1A,$18,$19,$1A,$18,$19,$1A,$18,$19,$1A,$18,$19,$1B,$15,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
 .db $00,$00,$00,$00,$00,$0A,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0C,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
 .db $01,$02,$03,$04,$07,$05,$30,$31,$32,$33,$34,$35,$36,$37,$0F,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
 .db $00,$00,$0D,$68,$69,$0E,$38,$39,$3A,$3B,$3C,$3D,$3E,$3F,$0F,$10,$11,$12,$13,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
 .db $1E,$18,$19,$1A,$18,$19,$1A,$18,$19,$1A,$18,$19,$1A,$18,$19,$1A,$18,$19,$1B,$15,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
 .db $00,$00,$00,$00,$00,$0A,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0C,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
 .db $01,$02,$03,$04,$08,$05,$40,$41,$42,$43,$44,$45,$46,$47,$0F,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
 .db $00,$00,$0D,$6C,$6D,$0E,$48,$49,$4A,$4B,$4C,$4D,$4E,$4F,$0F,$10,$11,$12,$13,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
 .db $1E,$18,$19,$1A,$18,$19,$1A,$18,$19,$1A,$18,$19,$1A,$18,$19,$1A,$18,$19,$1B,$15,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
 .db $00,$00,$00,$00,$00,$0A,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0C,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
 .db $01,$02,$03,$04,$09,$05,$50,$51,$52,$53,$54,$55,$56,$57,$0F,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
 .db $00,$00,$0D,$6E,$6F,$0E,$58,$59,$5A,$5B,$5C,$5D,$5E,$5F,$0F,$10,$11,$12,$13,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
 .db $1F,$18,$19,$1A,$18,$19,$1A,$18,$19,$1A,$18,$19,$1A,$18,$19,$1A,$18,$19,$1A,$18,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
 .db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

.ENDS
