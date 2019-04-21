
MML6.exe -i=%1 -o=../Song.mcs -t=gb
@if /I "%ERRORLEVEL%" NEQ "0" (
	@PAUSE
	@exit
)
@echo off
goto continue

# Run assmebly file with this program

:continue
echo [objects] > temp.prj
echo Output.obj >> temp.prj

echo on
wla-gb -v -o Output.obj musPlayer.asm
wlalink -v -S -r temp.prj musPlayer.gb
@echo off

del Output.obj
del temp.prj
PAUSE