WX_CONFIG ?= wx-config
CXX ?= g++
override CXXFLAGS += $(shell $(WX_CONFIG) --cxxflags) -Wall \
						-DVERSION=\"$(VERSION)\"
override LDFLAGS += $(shell $(WX_CONFIG) --libs) -L../../libr3r
override LIBS += -llibr3r_shared

ifdef DEBUG
override CXXFLAGS += -g
endif

CXXEXT = .cpp
HEXT = .h

objects = app desc events feedlist feedlistthread feedlistview \
					frame gofield htmldescriptionbox i18n location menu settings-display \
					settings-headers settings-http settings-main \
					settings-programs settings-subscriptions \
					settingscheckbox settingsdialog settingsentry \
					settingslist subscriptions

include ../../../scripts/common.make

ifdef inWindows
rcobjs = ../../../icons/r3r.res
else
rcobjs =
endif

all: r3r$(TARGETEXEEXT)
	cd po && $(MAKE) all

clean:
	cd po && $(MAKE) clean
	$(DEL) r3r$(EXEEXT)

install:
	cd po && $(MAKE) install

uninstall:
	cd po && $(MAKE) uninstall

r3r$(TARGETEXEEXT): $(addsuffix $(OEXT),$(objects))
	$(CXX) -o $@ $(LDFLAGS) $^ $(rcobjs) $(LIBS)

%$(OEXT): %$(CXXEXT)
	$(CXX) -c -o $@ $(CXXFLAGS) $<

%$(CXXEXT): %$(HEXT) wx.h
	$(TOUCH) $@

desc.cpp location.cpp settings-programs.cpp \
	settings-subscriptions.cpp: widgetids.h
	$(TOUCH) $@

events.cpp: eventtables.h libr3r.h widgetids.h
	$(TOUCH) $@

feedlist.h frame.h settingscheckbox.h subscriptions.h: libr3r.h
	$(TOUCH) $@

menu.cpp settingsentry.cpp: libr3r.h widgetids.h
	$(TOUCH) $@
