include ../scripts/common.make

WINDRES ?= $(call programpath,windres)

all: r3r.ico r3r.res

r3r.ico: r3r.png
	-png2ico $@ $<

r3r.res: r3r.rc
	-$(WINDRES) -i $< -o $@

clean:
	$(DEL) r3r.ico r3r.res

install:
	$(MKDIR) $(icondir)
	-$(INSTALL) r3r.ico r3r.png $(icondir)

uninstall:
	$(RM) $(icondir)/r3r.png $(icondir)/r3r.ico

