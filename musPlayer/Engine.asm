;Main loop

;AF is top of stack, carry is clear
;No more directives read when carry is set on stack
;Do not maintain Play pointer when calling
  PUSH DE
  PUSH AF
    RST $30       ;CALL HL
  POP AF
  POP DE
  JR nc,-

;Directive actions
;Calling convention
;A = Directive
;B = Data
;C = Channel IO base
;DE= Channel Memory base
;Preserve DE

;Note
_Note:
  PUSH AF       ;Saved for length calculation
  ;Channel 3 needs to be stopped to change its notes.
  ;Nothing else has anything on IO+$00 bit 7, so it's safe for all channels
    LDH A,(C)
    AND $7F       ;Preserve other bits
    LDH (C),A     ;Turn it off
    OR $80
    LDH (C),A     ;And on again
    LD HL,$002C   ;Octave offset
    ADD HL,DE
    LD B,(HL)
    AND $F0     ;Get absolute note
    SUB $40
    SWAP A
    ADD B
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
    LD B,(HL)
    LD HL,$0031 ;Stacatto
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
;  JR _SetLength        ;Fall through

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
  INC C
  INC C
  LDH A,(C)     ;Save envelope value
  LD B,A
  XOR A
  LDH (C),A     ;A zeroed envelope ends notes
  LD A,B
  LDH (C),A
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

;Tempo
_Tempo:
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
  INC (HL)
  LD A,B
  CP (HL)
  RET c ;If we have looped enough, don't follow the loop
;If we are here, we have not looped enough. Follow the loop
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
  LD HL,$0031
  ADD HL,DE
  LD B,$03
  AND A         ;Isolate tone
  RRCA
  RRCA
  LD B,A
  LD A,$3F
  AND (HL)
  OR B
  LD (HL),A     ;Merge tone and stacatto
  INC C
  LDH (C),A
  RET

;Tone (Channel 3)
;Load in a waveform from the table
_Tone3:
  ;XOR A     ;Directive alreay has high bit unset
  LDH (C),A     ;Turn off channel so we can change wave
  SWAP B
  LD A,$0F
  AND B
  LD C,A
  LD A,$F0
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
  LDH A,(C)     ;Include wave data
  OR B
  LD B,A
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
  RET
