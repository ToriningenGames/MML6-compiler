DEMOSONG=Test
CC=cc
MML=MML6\bin\MML6.exe

vpath %.asm .\musPlayer
vpath %.mml .\Songs
vpath %.obj .\$(OBJDIR)

OBJDIR=obj
LIBDIR=lib
OBJ=$(OBJDIR)\musPlayer.obj
LIB=$(addprefix $(LIBDIR)/,Sound.lib Voicelist.lib playerSongs.lib)
LINK=$(OBJDIR)\Link.link
SONGTARGET=$(OBJDIR)\Song.mcs

demo : $(LINK) $(SONGTARGET) $(OBJ) | $(OBJDIR) $(LIBDIR)
	wlalink -v -S -r $(LINK) musPlayer.gb

$(LINK) : Makefile | $(OBJDIR) $(LIBDIR)
	$(file > $(LINK),[objects])
	$(foreach I, $(OBJ),$(file >> $(LINK), $(I)))
	$(file >> $(LINK),[libraries])
	$(foreach I, $(LIB),$(file >> $(LINK),BANK 0 SLOT 0 $(I)))

$(OBJ) : musPlayer.asm Sound.asm Voicelist.asm playerSongs.asm $(SONGTARGET) | $(OBJDIR)
	wla-gb -v -I $(OBJDIR) -I musPlayer -o $@ $<

$(LIBDIR)/%.lib : %.asm | $(LIBDIR)
	wla-gb -v -I $(OBJDIR) -I musPlayer -l $@ $<

$(SONGTARGET) : $(addsuffix .mml,$(DEMOSONG)) $(MML) | $(OBJDIR)
	$(MML) -i=$< -o=$@ -t=gb

$(MML) :
	cd MML6 && $(MAKE) CC=$(CC)

$(OBJDIR) $(LIBDIR) :
	mkdir $@

clean :
	rmdir /S /Q $(OBJDIR)
	rmdir /S /Q $(LIBDIR)
	del /S /Q musPlayer.gb musPlayer.sym
	cd MML6 && $(MAKE) clean
