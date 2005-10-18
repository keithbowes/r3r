.POSIX: 

icondir = $(datarootdir)/icons

all: r3r.ico

distclean:
	rm -f r3r.ico

install:
	-mkdir $(icondir)
	$(INSTALL_DATA) r3r.png $(icondir)/r3r.png
	-$(INSTALL_DATA) r3r.ico $(icondir)/r3r.ico

uninstall:
	rm -f $(icondir)/r3r.png $(icondir)/r3r.ico

r3r.ico: r3r.png
	-png2ico r3r.ico r3r.png
	-windres -i r3r.rc -o r3r.res
