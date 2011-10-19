
FILTERDIR = $(pkglibdir)/Filters
CSC = $(MCS) -debug

TARGET = Filters.dll

CSFLAGS = -target:library $(BEAGLE_DEFINES)

# Found at http://sharptoolbox.madgeek.com/


CSFILES = 				\
	$(srcdir)/AssemblyInfo.cs	\
	$(srcdir)/FilterIgnore.cs	\
	$(srcdir)/FilterText.cs	        

########################################################

LOCAL_ASSEMBLIES =				\
	../Util/Util.dll			\
	../BeagleClient/Beagle.dll		\
	../beagled/BeagleDaemonPlugins.dll

ASSEMBLIES =				\
	$(BEAGLED_LIBS)			\
	$(SHARPZIPLIB_LIBS)		\
	$(TAGLIB_SHARP_LIBS)		\
	-r:Mono.Posix			\
	-r:System.Web			\
	$(LOCAL_ASSEMBLIES:%=-r:%)

########################################################

RESOURCES =

$(TARGET): $(CSFILES) $(LOCAL_ASSEMBLIES)
	$(CSC) -out:$@ $(CSFLAGS) $(CSFILES) $(ASSEMBLIES) $(RESOURCES)

all: $(TARGET)

install-data-local: $(TARGET)
	$(mkinstalldirs) $(DESTDIR)$(FILTERDIR)
	$(INSTALL_DATA) $(TARGET) $(TARGET).mdb $(DESTDIR)$(FILTERDIR)

uninstall-local:
	rm -f $(DESTDIR)$(FILTERDIR)/$(TARGET)
	rm -f $(DESTDIR)$(FILTERDIR)/$(TARGET).mdb

EXTRA_DIST =			\
	$(CSFILES)		\
	$(COND_CS_FILES)

CLEANFILES =		\
	$(TARGET)	\
	$(TARGET).mdb
