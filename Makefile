DEMOSONG=Test
CC=cc
MML=MML6\bin\MML6.exe

vpath %.asm .\musPlayer
vpath %.mml .\Songs
vpath %.obj .\$(OBJDIR)

OBJDIR=obj
OBJ=$(OBJDIR)\musPlayer.obj
LINK=$(OBJDIR)\Link.link
SONGTARGET=$(OBJDIR)\Song.mcs

demo : $(LINK) $(SONGTARGET) $(OBJ) | $(OBJDIR)
	wlalink -v -S -r $(LINK) musPlayer.gb

$(LINK) : Makefile | $(OBJDIR)
	$(file > $(LINK),[objects])
	$(foreach I, $(OBJ),$(file >> $(LINK), $(I)))

$(OBJ) : musPlayer.asm Engine.asm Voicelist.asm playerSongs.asm $(SONGTARGET) | $(OBJDIR)
	wla-gb -v -I $(OBJDIR) -I musPlayer -o $@ $<

$(SONGTARGET) : $(addsuffix .mml,$(DEMOSONG)) $(MML) | $(OBJDIR)
	$(MML) -i=$< -o=$@ -t=gb

$(MML) :
	cd MML6 && $(MAKE) CC=$(CC)

$(OBJDIR) :
	mkdir $@

clean :
	rmdir /S /Q obj
	del /S /Q musPlayer.gb musPlayer.sym
	cd MML6 && $(MAKE) clean
