OBJDIR=obj
BINDIR=bin
SRCDIR=src
SRCS_MML=CompileMML.c MML_gb.c MML_c64.c MML_MML.c MMLCore1.c Optimise.c
SRCS_COMPARE=Compare.c
SRCS=$(SRCS_MML) $(SRCS_COMPARE)
#Autodepends, a la https://make.mad-scientist.net/papers/advanced-auto-dependency-generation/

all : $(BINDIR)/MML6 $(BINDIR)/mmlcompare

DEPDIR=.deps
DEPFLAGS= -MT $@ -MMD -MP -MF $(DEPDIR)/$*.d
COMPILE.c = $(CC) $(DEPFLAGS) $(CFLAGS) $(CPPFLAGS) $(TARGET_ARCH) -c

$(OBJDIR)/%.o : $(SRCDIR)/%.c $(DEPDIR)/%.d | $(DEPDIR) $(OBJDIR)
	$(COMPILE.c) $(OUTPUT_OPTION) $<

DEPFILES := $(SRCS:%.c=$(DEPDIR)/%.d)
$(DEPFILES):

$(BINDIR)/MML6 : $(SRCS_MML:%.c=$(OBJDIR)/%.o) | $(BINDIR)
	$(CC) $(LDFLAGS) -o $@ $^

$(BINDIR)/mmlcompare : $(SRCS_COMPARE:%.c=$(OBJDIR)/%.o) | $(BINDIR)
	$(CC) $(LDFLAGS) -o $@ $^

clean :
	$(RM) -r $(OBJDIR) $(BINDIR)

$(DEPDIR) $(BINDIR) $(OBJDIR) : 
	mkdir $@

.PHONY : all clean

include $(wildcard $(DEPFILES))
