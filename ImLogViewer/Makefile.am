
CSC = $(MCS) -debug

TARGET = ImLogViewer.exe
WRAPPER = beagle-imlogviewer

CSFLAGS = -target:exe

CSFILES =				\
	$(srcdir)/ImLog.cs              \
	$(srcdir)/ImLogViewer.cs	\
	$(srcdir)/ImLogWindow.cs

LOCAL_ASSEMBLIES =			\
	../Util/Util.dll		\
	../Util/UiUtil.dll

ASSEMBLIES =				\
	$(BEAGLE_UI_LIBS)		\
	$(LOCAL_ASSEMBLIES:%=-r:%)	\
	-r:Mono.Posix

RESOURCEFILES =				\
	$(srcdir)/ImLogViewer.glade

# Makefile-fu to map /path/filename to -resource:/path/filename,filename
RESOURCES = $(join $(RESOURCEFILES:%=-resource:%,),$(notdir $(RESOURCEFILES)))

$(TARGET): $(CSFILES) $(LOCAL_ASSEMBLIES) $(RESOURCEFILES)
	$(CSC) -out:$@ $(CSFLAGS) $(CSFILES) $(ASSEMBLIES) $(RESOURCES)

all: $(TARGET) $(WRAPPER)

install-data-local: $(TARGET)
	$(mkinstalldirs) $(DESTDIR)$(bindir)
	$(mkinstalldirs) $(DESTDIR)$(pkglibdir)
	$(INSTALL_DATA) $(TARGET) $(TARGET).mdb $(DESTDIR)$(pkglibdir)
	sed -e "s|\#installed=1|installed=1|" < $(WRAPPER) > $(WRAPPER).tmp
	$(INSTALL_SCRIPT) $(WRAPPER).tmp $(DESTDIR)$(bindir)/$(WRAPPER)
	rm -f $(WRAPPER).tmp

uninstall-local:
	rm -f $(DESTDIR)$(pkglibdir)/$(TARGET)
	rm -f $(DESTDIR)$(pkglibdir)/$(TARGET).mdb
	rm -f $(DESTDIR)$(bindir)/$(WRAPPER)

$(WRAPPER): $(srcdir)/$(WRAPPER).in
	sed 					\
	-e "s|\@prefix\@|$(prefix)|g" 		\
	-e "s|\@pkglibdir\@|$(pkglibdir)|g"	\
	-e "s|\@target\@|$(TARGET)|g" 		\
	-e "s|\@gacprefix\@|$(GAC_PREFIX)|g" 	\
	-e "s|\@bash\@|$(BASH)|"		\
	< $^ > $@
	chmod +x $(WRAPPER)

EXTRA_DIST =			\
	$(CSFILES)		\
	$(WRAPPER).in		\
	$(RESOURCEFILES)

CLEANFILES =		\
	$(TARGET)	\
	$(TARGET).mdb	\
	$(WRAPPER)	\
	ImLogViewer.glade.h
