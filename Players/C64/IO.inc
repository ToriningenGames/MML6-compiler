;Common Defines for the C64

;CIAs

.STRUCT CIA_st
DataPortA       db
DataPortB       db
DirPortA        db
DirPortB        db
TimerA          dw
TimerB          dw
ClockTenths     db
ClockSecs       db
ClockMins       db
ClockHrs        db
ShiftReg        db
IntControl      db
TimControlA     db
TimControlB     db
.ENDST

.ENUM $DC00
CIA1    INSTANCEOF      CIA_st
.ENDE
.ENUM $DD00
CIA2    INSTANCEOF      CIA_st
.ENDE


;SID

.STRUCT SIDVoice_st
Frequency       dw
PulseWidth      dw
Control         db
ADSR            dw
.ENDST

.STRUCT SIDFilter_st
Frequency       dw
ResoEnable      db
ModeVol         db
.ENDST

.STRUCT SIDMisc_st
PotX            db
PotY            db
Oscillator      db
Envelope        db
.ENDST

.ENUM $D400
SIDch   INSTANCEOF SIDVoice_st  3
SIDflt  INSTANCEOF SIDFilter_st
SIDmisc INSTANCEOF SIDMisc_st
.ENDE


;VIC-II

.STRUCT Sprite_st
X       db
Y       db
.ENDST

.STRUCT VIC_st
Control1        db
Raster          db
LightPenX       db
LightPenY       db
SpriteOn        db
Control2        db
SpriteYExp      db
Memory          db
IntReg          db
IntEna          db
SpritePrio      db
SpriteMulti     db
SpriteXExp      db
SpriteSprColl   db
SpriteBakColl   db
ColBorder       db
ColBkg0         db
ColBkg1         db
ColBkg2         db
ColBkg3         db
ColSprShared0   db
ColSprShared1   db
ColSprite0      db
ColSprite1      db
ColSprite2      db
ColSprite3      db
ColSprite4      db
ColSprite5      db
ColSprite6      db
ColSprite7      db
.ENDST

.ENUM $D000
Sprite  INSTANCEOF Sprite_st    8
SpriteX db
VIC     INSTANCEOF VIC_st
.ENDE

.DEFINE ColorRam        $D800
