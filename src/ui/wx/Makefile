ifneq ($(HOST),)
WXFLAGS ?= --host=$(HOST)
endif

ifneq ($(DEBUG),1)
WX_CONFIG ?= wx-config $(WXFLAGS)
else
WX_CONFIG ?= $(if $(shell wx-config --cxx --debug),wx-config --debug $(WXFLAGS),wx-config $(WXFLAGS))
endif

CXX ?= $(shell $(WX_CONFIG) --cxx)
override CXXFLAGS += $(shell $(WX_CONFIG) --cxxflags) -Wall \
						-I../../libr3r
override LDFLAGS += $(shell $(WX_CONFIG) --libs) -L../../libr3r
override LIBS += -llibr3r_shared

ifeq ($(DEBUG),1)
override CXXFLAGS += -g
else
override LDFLAGS+=-s
ifdef forWindows
override LDFLAGS+=-mno-windows
endif
endif

ifneq ($(USE_NLS),0)
override CXXFLAGS += -DUSE_NLS
endif

CXXEXT = .cpp
HEXT = .h

objects = app desc events feedlist feedlistview \
					frame gofield htmldescriptionbox i18n location menu settings-display \
					settings-headers settings-http settings-main \
					settings-programs settings-subscriptions \
					settingscheckbox settingsdialog settingsentry \
					settingslist subscriptions

include ../../../scripts/common.make

ifdef forWindows
rcobjs = ../../../icons/r3r.res
else
rcobjs =
endif

all: r3r$(TARGETEXEEXT)
ifneq ($(USE_NLS),0)
	cd po && $(MAKE) all
endif

clean:
	cd po && $(MAKE) clean
	$(RM) $(wildcard r3r$(EXEEXT))

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

events.cpp: eventtables.h ../../libr3r/libr3r.h widgetids.h
	$(TOUCH) $@

feedlist.h frame.h settingscheckbox.h subscriptions.h: ../../libr3r/libr3r.h
	$(TOUCH) $@

menu.cpp settingsentry.cpp: ../../libr3r/libr3r.h widgetids.h
	$(TOUCH) $@
