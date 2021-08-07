DEMOSONG=Song.mml

OBJ=musPlayer.obj
LINK=Link.link

vpath %.asm musPlayer
vpath %.mml Songs

demo: $(LINK) $(OBJ)
	wlalink -v -S -r $(LINK) musPlayer.gb

$(LINK) : Makefile
	$(file > $(LINK),[objects])
	$(foreach I, $(OBJ),$(file >> $(LINK), obj/$(I)))

%.obj : %.asm
	wla-gb -v -o $@ $<

%.mcs : %.mml
	MML6 -i=$< -o=$@ -t=gb

MML6 :
	cd MML6; make