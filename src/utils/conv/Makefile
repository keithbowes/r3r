include ../../../scripts/common.make

CFLAGS=-DLOCDIR="\"$(localedir)\"" -I../../libr3r
LIBS=-lintl -llibr3r_shared
LDFLAGS=-L../../libr3r

all: r3r-conv$(TARGETEXEEXT)
	cd po && $(MAKE) all

r3r-conv$(TARGETEXEEXT): atomwriter$(OEXT) esfwriter$(OEXT) r3r-conv$(OEXT) rss3writer$(OEXT) rsswriter$(OEXT)
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $^ $(LIBS)

install:
	$(INSTALLEXE) r3r-conv$(TARGETEXEEXT) $(bindir)
	cd po && $(MAKE) install

uninstall:
	$(RM) $(bindir)/r3r-conv$(TARGETEXEEXT)
	cd po && $(MAKE) uninstall

clean: _clean
	$(DEL) r3r-conv$(TARGETEXEEXT)
	cd po && $(MAKE) clean

%$(OEXT): %.c
	$(CC) $(CFLAGS) -c -o $@ $<

%.c: %.h
	$(TOUCH) $@
