.POSIX:

ISCC = iscc

installer: setup.exe

setup.exe: r3r.iss
	$(ISCC) r3r.iss
	cp -f setup.exe ../../../../r3r-$(VERSION).exe

r3r.iss: _r3r.iss
	sed 's/@VERSION@/$(VERSION)/g' _r3r.iss > r3r.iss.1
	sed 's/@UI@/$(R3R_UI)/g' r3r.iss.1 > r3r.iss.2
	d=`pwd` && cd ../../../libr3r/po && index=`expr 2` && oldindex=$$index && \
		for lang in *.mo; do index=`expr $$index + 1`; l=`echo $$lang | sed \
		's/\.mo$$//g'`; \
		sed "s/;insert_next_lang/Source: \"libr3r\\\\po\\\\$$lang\"; DestDir: \"{app}\\\\share\\\\locale\\\\$$l\\\\LC_MESSAGES\"; DestName: \"libr3r.mo\"\n;insert_next_lang/g" \
		$$d/r3r.iss.$$oldindex > $$d/r3r.iss.$$index; oldindex=$$index; done && \
		cd ../../$(R3R_UI)/po && for lang in *.mo; do index=`expr $$index + 1`; \
		l=`echo $$lang | sed 's/\.mo$$//g'`; \
		sed "s/;insert_next_lang/Source: \"$(R3R_UI)\\\\po\\\\$$lang\"; DestDir: \"{app}\\\\share\\\\locale\\\\$$l\\\\LC_MESSAGES\"; DestName: \"r3r_$(R3R_UI).mo\"\n;insert_next_lang/g" \
		$$d/r3r.iss.$$oldindex > $$d/r3r.iss.$$index; oldindex=$$index; done && \
		cd $$d && mv r3r.iss.$$index r3r.iss && rm -f r3r.iss.*
