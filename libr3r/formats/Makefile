SHELL =sh

.SUFFIXES:
.SUFFIXES: $(OBJEXT) .pas

all: deps feeditem$(OBJEXT) feed$(OBJEXT) \
	nonxml$(OBJEXT) esf$(OBJEXT) rss3$(OBJEXT) \
       	xml$(OBJEXT) rss$(OBJEXT) atom$(OBJEXT)

clean:
	rm -f *$(OBJEXT) *$(UNITEXT)

deps:
	cd ../protocols && $(MAKE) formatdeps

.pas$(OBJEXT):
	$(PC) $(PCFLAGS) $(UNITDIR)../protocols $(UNITDIR)../regexpr $<
