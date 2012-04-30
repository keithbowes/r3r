include ../scripts/common.make

PNG2ICO ?= $(call programpath,png2ico)

WINDRES ?= $(call programpath,windres)
WINDRESFLAGS  ?= -O coff

all: r3r.ico r3r.res

r3r.ico: r3r.png
	-$(PNG2ICO) $@ $<

r3r.res: r3r.rc
	-$(WINDRES) $(WINDRESFLAGS) -i $< -o $@

clean:
	$(DEL) r3r.ico r3r.res

install:
	$(MKDIR) $(icondir)
	-$(INSTALL) r3r.ico r3r.png $(icondir)

uninstall:
	$(RM) $(icondir)/r3r.png $(icondir)/r3r.ico

