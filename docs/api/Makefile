include ../../scripts/common.make

FPDOC ?= $(call programpath,fpdoc)

all: index.html

index.html: libr3r.pas.xml ../../src/libr3r/formats/feeditem.pas \
	 ../../src/libr3r/libr3r.pas ../../src/libr3r/rmessage.pas
	-$(FPDOC) --input=../../src/libr3r/libr3r.pas \
		--input=../../src/libr3r/formats/feeditem.pas \
		--input=../../src/libr3r/rmessage.pas \
		--input=../../src/libr3r/rsubscriptions.pas \
		--input=../../src/libr3r/settings/rsettings.pas \
		--input=../../src/libr3r/formats/itemcallbacks.pas \
		--descr=libr3r.pas.xml --package=LibR3R --format=html

clean:
	$(RM) $(wildcard *.css *.html *.png)
	$(RM) $(wildcard tree.xml)
	$(RMRF) $(wildcard feeditem itemcallbacks libr3r rmessage rsettings rsubscriptions)

install: all
	$(MKDIR) $(docdir)
	$(CPR) LibR3R $(docdir)/api

uninstall:
	$(RMRF) $(wildcard $(docdir))
