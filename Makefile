MML=MML6/bin/MML6

PLAYERS=$(addprefix Players/,GB)

all :
	cd MML6 && $(MAKE) CC=$(CC)
	$(foreach target,$(PLAYERS),cd $(target) && $(MAKE); )

clean :
	cd Players/GB && $(MAKE) clean
	cd MML6 && $(MAKE) clean
