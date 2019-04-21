DEL := del
DFLAGS := -Wall -Wextra -Wno-implicit-fallthrough -Og -g3
RFLAGS := -O1 -DNDEBUG
CCFLAGS := --std=c11 -c 
LDFLAGS :=  
ONAME := a.out
#Where the source files are
SRCPRE := .\\
#Where the output should go
DSTPRE := .\\
DEBUGPRE := debug-
#The name of the output compiler
DST := MML6.exe

#Add your source file here
TARGETLIST := MML_MML.c MML_gb.c
OBJLIST := $(TARGETLIST:.c=.o)
CORELIST := CompileMML.o MMLCore1.o Optimise.o

all: | target target-core
debug: | target-debug target-core

target:
	$(eval CCFLAGS := $(CCFLAGS) $(RFLAGS))
	$(eval DEBUGPRE := )
	$(eval ONAME := $(DSTPRE)$(DST))
	$(eval TARGETS := $(CORELIST) $(OBJLIST))
%.o: %.c
	$(CC) $(CCFLAGS) $(SRCPRE)$*.c -o $(DEBUGPRE)$*.o
target-debug:
	$(eval CCFLAGS := $(CCFLAGS) $(DFLAGS))
	$(eval ONAME := $(DSTPRE)$(DEBUGPRE)$(DST))
	$(eval TARGETS := $(addprefix $(DEBUGPRE),$(CORELIST) $(OBJLIST)))
target-core: $(CORELIST) $(OBJLIST)
	$(CC) $(LDFLAGS) $(TARGETS) -o $(ONAME)
clean:
	$(DEL) $(CORELIST) $(OBJLIST)
	$(eval TARGETS := $(addprefix $(DEBUGPRE),$(CORELIST) $(OBJLIST)))
	$(DEL) $(TARGETS)
