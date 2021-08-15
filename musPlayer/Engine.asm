;Sound player 5.0

.define channelonebase          $CE04
.define channeltwobase          $CE36
.define channelthreebase        $CE68
.define channelfourbase         $CE9A
.define channelcontrolbase      $CECC
.define channelsize $32
.define musicglobalbase $CE00
.export channelonebase
.export channeltwobase
.export channelthreebase
.export channelfourbase
.export channelcontrolbase
.export musicglobalbase
.export channelsize


;See MMLspec.txt for binary format of music data
;Memory map:
    ;Global:
    ;+$00: 1 byte : Sound effect control (don't use)
        ;%12341234
        ; ||||++++--- Sound effect requested
        ; ++++------- Sound effect playing
    ;+$01: 1 byte : Control register
        ;%1234NEQS
        ; |||||||+--- All sound on
        ; ||||||+---- Music on
        ; |||||+----- Enable sound effects (keep 0, don't use)
        ; ||||+------ New song
        ; ++++------- Channel N on
    ;+$02: 2 bytes: song pointer
    ;Contains per channel:
    ;+$00: 3 bytes: 8 loops + counter
    ;+$18: 1 byte : 16 lengths
    ;+$28: 2 bytes: Play pointer
    ;+$2A: 2 bytes: note table pointer
    ;+$2C: 1 byte : Octave offset
    ;+$2D: 1 byte : remaining note length
    ;+$2E: 1 byte : tempo quotient value
    ;+$2F: 1 byte : tempo remainder value
    ;+$30: 1 byte : tempo remainder counter
    ;+$31: 1 byte : Stacatto value

;Use:
    ;Call MusicLoad with BC->the song file to prepare the song
    ;Set the control register (see Memory Map) to play music
    ;Have interrupts enabled
;Switching songs:
    ;Just call MusicLoad again, and have interrupts enabled.

;Setup:
    ;Include this file in your project
    ;Call PlayTick at the end of your vBlank routine
;More detailed setup:
    ;PlayChannel has to be called with DE==the beginning of channel data for
    ;each channel about 60 times a second. Varying this will affect the tempo
    ;of the channels

;This player uses 256 bytes. It relies on this block not crossing a page boundry

.SECTION "Music Engine" FREE

MusicLoad:
;BC->music file
  LD HL,musicglobalbase+$03
  LD (HL),B
  DEC L
  LD (HL),C
  DEC L
  SET 3,(HL)    ;Indicate new song
  RET

_InitializeChannel:
;Set Play Pointer to value pointed to by HL, added to BC
;Set Octave to 2    (octave 4)
;Set Tempo Quotient to 2    (Tempo 120)
;Set Tempo Remainder to 0
;Set Stacatto to 0  (no length enable)
;Set Remaining Length to 0
;Set Remainder Counter to 0
  PUSH HL
  PUSH BC
    LDI A,(HL)
    LD H,(HL)
    LD L,A
    ADD HL,BC
    LD B,H
    LD C,L
    LD HL,$0028   ;Play pointer
    ADD HL,DE
    LD (HL),C
    INC L
    LD (HL),B
    INC L
    INC L
    INC L       ;Octave offset
    XOR A
    LD (HL),2
    INC L       ;Remaining note length
    LDI (HL),A
    LD (HL),2   ;Tempo quotient
    INC L
    LDI (HL),A  ;Tempo remainder
    LDI (HL),A  ;Tempo counter
    LDI (HL),A  ;Stacatto
  POP BC
  POP HL
  INC HL        ;Point to next entry
  INC HL
  RET

PlayTick:
  LD A,(musicglobalbase+$01)
  RRCA
  LDH ($26),A   ;Master sound enable bit
  RET nc
  BIT 0,A       ;Music enable bit
  RET nc
  BIT 2,A       ;New Song
  JR z,+
  ;Set new song
  RES 2,A
  RLCA
  LD HL,musicglobalbase+$01     ;Control register
  LDI (HL),A    ;Move HL to new song pointer
  LD C,(HL)
  INC L
  LD B,(HL)
  LD H,B
  LD L,C
  LD DE,channelcontrolbase
  CALL _InitializeChannel
  LD E,<channelonebase
  CALL _InitializeChannel
  LD E,<channeltwobase
  CALL _InitializeChannel
  LD E,<channelthreebase
  CALL _InitializeChannel
  LD E,<channelfourbase
  CALL _InitializeChannel
+
  LD DE,channelcontrolbase
  CALL _PlayChannel
  LD A,(musicglobalbase+$01)
  BIT 4,A
  JR z,+
  LD E,<channelonebase
  CALL _PlayChannel
+
  LD A,(musicglobalbase+$01)
  BIT 5,A
  JR z,+
  LD E,<channeltwobase
  CALL _PlayChannel
+
  LD A,(musicglobalbase+$01)
  BIT 6,A
  JR z,+
  LD E,<channelthreebase
  CALL _PlayChannel
+
  LD A,(musicglobalbase+$01)
  BIT 7,A
  RET z
  LD E,<channelfourbase
;  JR _PlayChannel      Fall through

;Main per channel loop
_PlayChannel:
;DE= Pointer to channel data
  LD HL,$002F
  ADD HL,DE
  LDI A,(HL)    ;Accumulate remainder
  ADD (HL)
  LDD (HL),A
  DEC L
  DEC L
  LDI A,(HL)    ;Remaining note length
  SBC (HL)      ;Tick down remaining note length, with carry from fractional
  DEC L
  LD (HL),A
;Carry set right from subtract
  RET nc
--
;Do play notes
  LD HL,$0028   ;Play pointer
  ADD HL,DE
  LDI A,(HL)
  LD H,(HL)
  LD L,A
  LDI A,(HL)
  LD C,A
  AND $E0       ;Check for second byte
  JR nz,+
  LDI A,(HL)
  LD B,A
+
  PUSH BC
    LD B,H      ;Store new play pointer
    LD C,L
    LD HL,$0028
    ADD HL,DE
    LD (HL),C
    INC L
    LD (HL),B
  POP BC
  LD A,C
  LD C,-1
  PUSH AF       ;Directive is ready, carry clear from preceding AND
    LD A,E      ;Calculate addresses for this channel (IO/Function)
-
    INC C
    SUB channelsize
    JR nc,-
    LD A,C      ;Multiply by 5 for IO addresses
    ADD A
    ADD A
    ADD C
    LD C,A
    ADD A       ;Multiply by another 4 for function table address (total 20)
    ADD A
    ADD <_Channel1Directives
    LD L,A
    LD A,0
    ADC >_Channel1Directives
    LD H,A
    LD A,C
    ADD $10     ;IO base
    LD C,A
  POP AF
  PUSH AF       ;Determine which function in table to run
  PUSH BC
    LD B,H
    LD C,L
    LD HL,_DirectiveBoundTable
-
    CP (HL)
    INC HL
    JR nc,+
    ;Not this function
    INC BC
    INC BC
    JR -
+      ;Use this function
    LD A,(BC)   ;Get function address
    INC BC
    LD L,A
    LD A,(BC)
    LD H,A
  POP BC
  POP AF
;AF is top of stack, carry is clear
;No more directives read when carry is set on stack
;Do not maintain Play pointer when calling
  PUSH AF
    RST $30       ;CALL HL
  POP AF
  JR nc,--
  RET

;Directive list
;Order is important
;Lowest values for a given directive
_DirectiveBoundTable:
 .db $40,$30,$20,$10,$08,$04,$03,$02,$01,$00
;Function to call, given a directive
_Channel1Directives:
 ;Lowest value  ;Function for directive
 .dw _Note,      _Rest,      _Octave, _Length
 .dw _Loop,      _Tone,      _Tempo,  _Stacatto
 .dw _Envelope,  _Sweep
_Channel2Directives:
 .dw _Note,      _Rest,      _Octave, _Length
 .dw _Loop,      _Tone,      _Tempo,  _Stacatto
 .dw _Envelope,  _None
_Channel3Directives:
 .dw _Note3,     _Rest3,     _Octave, _Length
 .dw _Loop,      _Tone3,     _Tempo,  _Stacatto3
 .dw _Envelope3, _None
_Channel4Directives:
 .dw _Note4,     _Rest,      _Octave, _Length
 .dw _Loop,      _None,      _Tempo,  _None
 .dw _None,      _None
_Channel0Directives:
 .dw _Note,      _SetLength, _None,   _Length
 .dw _Loop,      _None,      _Tempo,  _None
 .dw _Envelope0, _None

;Directive actions
;Calling convention
;A = Directive
;B = Data
;C = Channel IO base
;DE= Channel Memory base
;Preserve DE

;Note
_Note3:
  PUSH AF       ;Saved for length calculation
  ;Channel 3 needs to be stopped to change its notes.
    LDH A,(C)
    AND $7F       ;Preserve other bits
    LDH (C),A     ;Turn it off
    OR $80
    LDH (C),A     ;And on again
  POP AF
_Note:
  PUSH AF
    LD HL,$002C   ;Octave offset
    ADD HL,DE
    AND $F0     ;Get absolute note
    SUB $40
    SWAP A
    LD B,A
    LD A,(HL)   ;Multiply octave by 12
    ADD A
    ADD (HL)
    ADD A
    ADD A
    ADD B       ;Add in note
    ADD A       ;Table entries are 2 bytes
    LD HL,$002A     ;Note table pointer
    ADD HL,DE
    ADD (HL)
    LD B,A
    INC L
    LD A,0
    ADC (HL)
    LD H,A
    LD L,B
    LDI A,(HL)  ;Note data
    INC C
    INC C
    INC C
    LDH (C),A   ;Frequency lo
    LD A,$3F    ;Don't have high two bits set, even if the table does
    AND (HL)
    LD B,A
    LD HL,$0031 ;Stacatto
    ADD HL,DE
    LD A,(HL)
    OR A
    LD A,B
    JR z,+      ;Length bit check
    ;Enable length bit only if a nonzero stacatto is set
    OR $40
+
    OR $80      ;Play note bit
    INC C
    LDH (C),A
  POP AF
;Calculate note length
  JR _SetLength

_Note4:
  LD HL,$002C   ;Octave Offset
  ADD HL,DE
  PUSH AF
    LD A,(HL)   ;Multiply octave by 12 (%00001100) (octaves -> half steps)
    LD B,A
    ADD A
    ADD B
    ADD A
    ADD A
    LD B,A
    DEC L       ;Note table pointer
    LDD A,(HL)
    LD L,(HL)
    LD H,A
  POP AF
  PUSH BC
  PUSH AF
    AND $F0     ;Get note to half steps from table base
    SUB $40
    SWAP A
    ADD B
    PUSH BC
    PUSH AF
      ADD A       ;2 bytes an entry
      ADD L
      LD L,A
      LD A,0
      ADC H
      LD H,A
      INC HL    ;Envelope data
      LD B,(HL)
      CALL _Envelope
      LD HL,$002A       ;Note table pointer
      ADD HL,DE
      LDI A,(HL)
      LD H,(HL)
      LD L,A
      LD BC,24*5        ;Skip from pitch to stacatto
      ADD HL,BC
    POP AF      ;Half step count
    POP BC      ;C is the IO register
    ADD L       ;Go to this note's stacatto
    LD L,A
    LD A,0
    ADC H
    LD H,A
    LD B,(HL)
    CALL _Stacatto
  POP AF
  POP BC
  JR _Note      ;Play the note like normal

;Update the Remaining Note Length
_SetLength:
  POP BC    ;Return
  POP HL    ;AF value
    AND $0F       ;Isolate length
    ADD $18       ;Length table base
    LD L,A
    LD H,$00
    ADD HL,DE
    LD A,(HL)     ;Grab this note's length
    LD HL,$2D     ;Remaining Note Length
    ADD HL,DE
    ADD (HL)
    LD (HL),A
  PUSH AF   ;New AF value
  LD H,B    ;Return
  LD L,C
  JP HL

;Rest
_Rest:
  LD L,A
  INC C
  INC C
  LDH A,(C)     ;Save envelope value
  LD B,A
  XOR A
  LDH (C),A     ;A zeroed envelope ends notes
  LD A,B
  LDH (C),A
  LD A,L        ;Directive
;Calculate note length
  JR _SetLength

;Rest (Channel 3)
;Channel 3 has an alternative way of stopping the note
_Rest3:
  ;CPL      Rest directives already have the high bit unset
  LDH (C),A     ;Turn it off
  CPL   ;Fast high bit toggle
  LDH (C),A     ;And on again
  CPL   ;Recover initial value
;Calculate note length
  JR _SetLength

;Tie
_Tie:
  LD HL,$0028   ;Play pointer
  ADD HL,DE
  INC (HL)      ;Update play pointer to sit past note
  LDI A,(HL)    ;Retrieve the play pointer
  LD H,(HL)     ;So we can tie the next note
  LD L,A
  DEC HL
  LD A,(HL)
  AND $E0       ;Directive may be a length directive (kinda illegal, but useful)
  LDI A,(HL)
  JR nz,_SetLength
  LD B,(HL)     ;Length data
  CALL _Length
  LD HL,$0028   ;Play pointer
  ADD HL,DE
  INC (HL)      ;Skip this data as well
  JR _Tie

;Tempo
_Tempo:
  XOR A         ;Tie check
  OR B
  JR z,_Tie
  ;Convert from BPM to ticks/frame by multiplying by 4/225 in fixed point 8.16
  ;This corresponds to a constant of $048B, or %0000010010001101
  ;HL has a convenient 16 bit ADD for us
  ;Carry outs are stored in A, being our integer portion
  ;The upper 8 bits of the fractional portion are stored
  ;Follow along! Start with 1, and shift in and toggle the bits!
  XOR A
  LD L,B
  LD H,A
  LD C,B        ;C is unneeded
  LD B,H
  ADD HL,HL     ;0
  ADD HL,HL     ;0
  ADD HL,HL
  ADD HL,BC     ;1
  ADD HL,HL     ;0
  ADD HL,HL     ;0
  ADD HL,HL     ;0
  ADD HL,HL
  ADD HL,BC     ;1
  ADD HL,HL
  ADD HL,BC     ;1
  ADD HL,HL     ;0
  ADC A         ;First possible carry out of H
  ADD HL,HL
  ADC A
  ADD HL,BC     ;1
  ADC 0
  LD B,H
  LD HL,$002E
  ADD HL,DE
  LDI (HL),A    ;Integer
  LD (HL),B     ;Fraction
  RET

;Loop
_Loop:
  BIT 7,B
  JR nz,_LoopGo
;  JR _LoopSet  Fall through
_LoopSet:
  PUSH AF
    LD HL,$0028   ;Play pointer
    ADD HL,DE
    LDI A,(HL)
    ADD B         ;Include jump offset
    LD C,A
    LD A,(HL)
    ADC 0
    LD B,A
    DEC BC      ;Subtract two to adjust for this directive
    DEC BC
  POP AF
  AND $07       ;Go to appropriate loop data
  LD L,A        ;Multiply by 3
  ADD A
  ADD L
  LD L,A
  LD H,$00
  ADD HL,DE
  LD (HL),C     ;Store destination pointer
  INC L
  LD (HL),B
  INC L
  LD (HL),0     ;Zero loop counter
  RET

_LoopGo:
  RES 7,B
  AND $07       ;Go to appropriate loop data
  LD L,A        ;Multiply by 3
  ADD A
  ADD L
  ADD 2         ;Go to counter specifically
  LD L,A
  LD H,$00
  ADD HL,DE
  LD A,B
  OR A
  JR z,+        ;Always loop when specified count==0
  INC (HL)
  CP (HL)
  RET c ;If we have looped enough, don't follow the loop
+       ;If we are here, we have not looped enough. Follow the loop
  DEC L
  LDD A,(HL)
  LD C,(HL)
  LD HL,$0029   ;Play pointer + 1
  ADD HL,DE
  LDD (HL),A
  LD (HL),C
  RET

;Tone
_Tone:
  LD HL,$0031   ;Stacatto (stacks with wave data)
  ADD HL,DE
  LD B,$03
  AND B         ;Isolate tone
  RRCA
  RRCA
  LD B,A
  LD A,$3F
  AND (HL)
  OR B
  INC C
  LDH (C),A
  RET

;Tone (Channel 3)
;Load in a waveform from the table
_Tone3:
  ;XOR A     ;Directive alreay has high bit unset
  LDH (C),A     ;Turn off channel so we can change wave
  SWAP B
  LD A,$F0
  AND B
  LD C,A
  LD A,$0F
  AND B
  LD B,A
  LD HL,Wave    ;Wave table
  ADD HL,BC
  LD C,$30      ;Wave data
  LD B,$10      ;Wave width
-
  LDI A,(HL)
  LDH (C),A
  INC C
  DEC B
  JR nz,-
  LD A,$FF      ;Reenable channel
  LDH ($1A),A
  RET

;Envelope (Channel 3)
;Sets a particular volume level on an inverse scale from the other channels
_Envelope3:
  ;Channel 3 volume levels scale nonlinearly,
  ;and matching volumes with other channels must accomodate for their
        ;nonlinear values as well as Channel 3's
  ;Map:
  ;%0000..%0010 -> %0000    (Vol   0%)
  ;%0011..%0101 -> %0110    (Vol  25%)
  ;%0110..%1010 -> %0100    (Vol  50%)
  ;%1011..%1111 -> %0010    (Vol 100%)
  INC C ;Move to envelope
  INC C
  LD A,B
  SUB %00110000
  JR nc,+
  XOR A
  LDH (C),A
  RET
+
  SUB %00110000
  JR nc,+
  LD A,%01100000
  LDH (C),A
  RET
+
  SUB %01010000
  JR nc,+
  LD A,%01000000
  LDH (C),A
  RET
+
  LD A,%00100000
  LDH (C),A
  RET

;Envelope (Channel 0)
;Envelope have the different function of Master Volume on the control channel
_Envelope0:
  RET

;Envelope
_Envelope:
  INC C
  INC C
  JR _SetIO

;Stacatto
_Stacatto:
  LD A,$3F
  AND B
  LD B,A
  LD HL,$0031
  ADD HL,DE
  LD (HL),B
  INC C
  LDH A,(C)     ;Include wave data when writing
  AND $C0
  OR B
  LDH (C),A
  RET

;Stacatto (Channel 3)
;All bits are used for sweep data, and there is no wave to consider
_Stacatto3:
  LD HL,$0031
  ADD HL,DE
  LD (HL),B
  INC C
;  JR _SetIO     ;Fall through
;Sweep
_Sweep:
;IO already at the right point
_SetIO:
  LD A,B
  LDH (C),A
  RET

;Length
_Length:
  AND $0F       ;Index
  ADD $18       ;Length table base
  LD L,A
  LD H,$00
  JR _SetMem

;Octave
_Octave:
  LD HL,$002C   ;Octave offset
  AND $0F
  DEC A         ;Note table starts on ocatve 2
  DEC A
  LD B,A
;  JR _SetMem    ;Fall through
_SetMem:
;General data edit
  ADD HL,DE
  LD (HL),B
_None:
  RET

.ENDS
