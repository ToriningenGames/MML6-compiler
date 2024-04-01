;Player

.MEMORYMAP
DEFAULTSLOT 0
SLOTSIZE $1000
SLOT 0 $C000
.ENDME

.ROMBANKMAP
BANKSTOTAL 1
BANKSIZE $1000
BANKS 1
.ENDRO

.BANK 0 SLOT 0
.ORGA $C000
Start:
  JSR SoundInit
  LDA #>SongData
  LDX #<SongData
  JSR NewSong
  RTS   ;Basic interactivity test
  CLC
-
  BCC -
  .db $F2       ;JAM


.SECTION "Song" FREE
SongData:
.INCBIN Song.mcs
.ENDS
