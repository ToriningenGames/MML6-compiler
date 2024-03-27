DEMOSONG=../../Songs/Test.MML
MML=../../MML6/bin/MML6

OBJDIR=obj
LIBDIR=lib

OBJ=$(OBJDIR)/musPlayer.obj
LIB=$(addprefix $(LIBDIR)/,Sound.lib Voicelist.lib playerSongs.lib)
LINK=$(OBJDIR)/Link.link
SONGTARGET=$(OBJDIR)/Song.mcs

demo : $(LINK) $(SONGTARGET) $(OBJ) $(LIB) | $(OBJDIR) $(LIBDIR) $(MML)
	wlalink -v -S -r $(LINK) musPlayer.gb

$(LINK) : Makefile | $(OBJDIR) $(LIBDIR)
	$(file > $(LINK),[objects])
	$(foreach I, $(OBJ),$(file >> $(LINK), $(I)))
	$(file >> $(LINK),[libraries])
	$(foreach I, $(LIB),$(file >> $(LINK),BANK 0 SLOT 0 $(I)))

$(OBJ) : musPlayer.asm | $(OBJDIR)
	wla-gb -v -I $(OBJDIR) -I musPlayer -o $@ $<

$(LIBDIR)/playerSongs.lib : $(SONGTARGET)
$(LIBDIR)/%.lib : %.asm | $(LIBDIR)
	wla-gb -v -I $(OBJDIR) -I musPlayer -l $@ $<

$(SONGTARGET) : $(DEMOSONG) | $(OBJDIR) $(MML)
	$(MML) -i=$< -o=$@ -t=gb

$(OBJDIR) $(LIBDIR) :
	mkdir $@

clean :
	$(RM) -r $(OBJDIR)
	$(RM) -r $(LIBDIR)
	$(RM) musPlayer.gb musPlayer.sym
