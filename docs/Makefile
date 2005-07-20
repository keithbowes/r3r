.POSIX: 

docsdir = $(datarootdir)/doc
docdir = $(docsdir)/r3r

all: libr3r.pas.xml ../libr3r/formats/feeditem.pas ../libr3r/libr3r.pas \
	../libr3r/rmessage.pas
	-fpdoc --input=../libr3r/libr3r.pas --input=../libr3r/formats/feeditem.pas \
		--input=../libr3r/rmessage.pas --descr=libr3r.pas.xml --package=LibR3R

install: all
	-mkdir $(docsdir)
	-mkdir $(docdir)
	-for doc in *.html */*.html; do mkdir `dirname $(docdir)/$$doc`; \
		$(INSTALL_DATA) $$doc $(docdir)/$$doc; done

uninstall:
	rm -fr $(docdir)
