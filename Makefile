DEMOSONG=Test
CC=cc

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

$(OBJDIR)\\%.obj : %.asm | $(OBJDIR)
	wla-gb -v -I $(OBJDIR) -I musPlayer -o $@ $<

$(SONGTARGET) : $(addsuffix .mml,$(DEMOSONG)) MML6\MML6 | $(OBJDIR)
	MML6\MML6 -i=$< -o=$@ -t=gb

MML6\MML6 :
	cd MML6 && make CC=$(CC)

$(OBJDIR) :
	mkdir $@

clean :
	rmdir /S /Q obj
	del /S /Q musPlayer.gb musPlayer.sym
