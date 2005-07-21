.POSIX: 

.SUFFIXES:
.SUFFIXES: $(OBJEXT) .pas

all: deps feeditem$(OBJEXT) feed$(OBJEXT) esf$(OBJEXT) xml$(OBJEXT) \
	rss$(OBJEXT) rss3$(OBJEXT) atom$(OBJEXT)

clean:
	rm -f *$(OBJEXT) *$(UNITEXT)

deps:
	cd ../protocols && $(MAKE) formatdeps

.pas$(OBJEXT):
	$(PC) $(PCFLAGS) $(UNITDIR)../protocols $(UNITDIR)../regexpr $<
