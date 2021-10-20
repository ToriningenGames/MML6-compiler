OBJDIR=obj/
BINDIR=bin/
SRCDIR=src/
override CFLAGS += --std=c11 -c
RFLAGS= -O1 -DNDEBUG
DFLAGS= -Wall -Wextra -Wno-implicit-fallthrough -Og -g3
LDFLAGS= 
BIN=MML6.exe
TARGETSRC=MML_MML MML_gb
CORESRC=CompileMML MMLCore1 Optimise
SRC=$(CORESRC) $(TARGETSRC)
CC=cc

vpath %.c $(SRCDIR)

.SUFFIXES:

.PHONY : all release debug clean common compare

all : release debug

release : CFLAGS:=$(CFLAGS) $(RFLAGS)
release : PRE:=
release :
	$(MAKE) '$(BINDIR)$(BIN)' CFLAGS='$(CFLAGS)'
	$(MAKE) 'compare' CFLAGS='$(CFLAGS)'

debug : CFLAGS:=$(CFLAGS) $(DFLAGS)
debug : PRE:=debug-
debug :
	$(MAKE) '$(BINDIR)$(PRE)$(BIN)' CFLAGS='$(CFLAGS)' PRE='$(PRE)'
	$(MAKE) 'compare' CFLAGS='$(CFLAGS)' PRE='$(PRE)'

compare : $(BINDIR)$(PRE)mmlcompare.exe

$(BINDIR)$(PRE)mmlcompare.exe : $(OBJDIR)$(PRE)Compare.o
	$(CC) $(LDFLAGS) $^ -o $@

$(BINDIR)$(PRE)$(BIN) : $(addprefix $(OBJDIR)$(PRE),$(addsuffix .o,$(SRC)))
	$(CC) $(LDFLAGS) $^ -o $@

$(OBJDIR)$(PRE)%.o : %.c
	$(CC) $(CFLAGS) $< -o $@

bin obj :
	mkdir $@

clean :
	del /S /Q bin obj
