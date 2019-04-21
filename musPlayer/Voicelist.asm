;Voicelist

.SECTION "Voices" ALIGN 16 FREE
Wave:

;First four match the output of channels 1 and 2
;0 Duty 0
.db $00,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF

;1 Duty 1
.db $00,$00,$00,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF

;2 Duty 2 (Square)
.db $00,$00,$00,$00,$00,$00,$00,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF

;3 Duty 3
.db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$FF,$FF,$FF,$FF

;4 Square (double octave)
.db $00,$00,$00,$00,$88,$88,$88,$88,$77,$77,$77,$77,$FF,$FF,$FF,$FF

;5 Square (Fourth, three octaves up)
.db $00,$00,$7F,$FF,$88,$00,$77,$77,$88,$88,$F7,$77,$00,$88,$FF,$FF

;6 Triangle (With octave up)
.db $13,$57,$8A,$CD,$FE,$ED,$CC,$BA,$AA,$BC,$CD,$EE,$EC,$A8,$64,$20

;7 Triangle (double octave, for bass and tenor)
.db $02,$46,$8A,$CE,$EC,$A8,$64,$20,$13,$57,$9B,$DF,$DB,$97,$53,$10

;8 Triangle (x8 octave, for high treble)
.db $08,$F8,$08,$F8,$08,$F8,$08,$F8,$08,$F8,$08,$F8,$08,$F8,$08,$F8

;9 Saw
.db $00,$11,$22,$33,$44,$55,$66,$77,$88,$99,$AA,$BB,$CC,$DD,$EE,$FF

;10 Saw (triple octave)
.db $02,$46,$89,$AC,$EF,$01,$35,$78,$9B,$DF,$01,$24,$56,$89,$AC,$DE

;11 Sine
.db $00,$11,$24,$56,$89,$BC,$DF,$FF,$FF,$FF,$ED,$BA,$86,$54,$22,$10

;12 Sawtooth Fifth
.db $87,$5C,$DA,$89,$2C,$B6,$7F,$EB,$42,$18,$95,$4D,$68,$62,$3B,$97

;13 Sawtooth Fifth (truncated)
.db $8F,$7E,$5B,$CC,$D2,$A1,$88,$99,$25,$C4,$B5,$66,$78,$F7,$EA,$BB

;14 Master Spark (plays at 110 Hz. Set NR33/34 to $5AC)
.db $01,$34,$42,$16,$64,$BF,$EE,$EB,$68,$99,$92,$16,$62,$6B,$66,$42

;15 Alien
.db $0F,$0E,$0D,$0C,$0B,$0A,$09,$08,$07,$06,$05,$04,$03,$02,$01,$00
.db $0B,$0A,$09,$08,$0B,$0A,$09,$08,$07,$06,$05,$04,$03,$02,$01,$00

;17 Error sound
.db $04,$85,$5B,$A4,$55,$B7,$D8,$DF,$9C,$ED,$B7,$AD,$83,$93,$F6,$4D

;18 Weird (sounds like a bad TV)
.db $0F,$1E,$2D,$3C,$4B,$5A,$69,$78,$87,$96,$A5,$B4,$C3,$2D,$1E,$0F

;19 Square and triangle
.db $01,$12,$23,$34,$45,$56,$67,$78,$FE,$ED,$DC,$CB,$BA,$A9,$98,$87

;20 Square, triangle, saw
.db $00,$12,$23,$44,$55,$66,$78,$89,$FF,$FE,$EE,$ED,$DD,$DD,$DC,$CC

;21 Square, triangle, loud saw
.db $01,$12,$33,$45,$56,$77,$88,$9A,$FE,$FE,$FE,$FE,$FE,$FE,$FE,$FE

;22 Square, triangle, reverse saw
.db $01,$11,$12,$23,$33,$34,$45,$55,$FE,$DC,$BA,$98,$76,$54,$32,$10

;23 Rounded Square
.db $40,$00,$00,$00,$00,$00,$00,$04,$CF,$FF,$FF,$FF,$FF,$FF,$FF,$FC

.ENDS
