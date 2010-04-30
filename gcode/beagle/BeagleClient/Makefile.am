CSC = $(MCS) -debug

TARGET = Beagle.dll

CSFLAGS = 						\
	-target:library					\
	$(BEAGLE_DEFINES)

if ENABLE_AVAHI
CSFLAGS += -define:ENABLE_AVAHI
endif

CSFILES = 						\
	$(srcdir)/AssemblyInfo.cs			\
	$(srcdir)/Hit.cs                		\
	$(srcdir)/IIndexableGenerator.cs       		\
	$(srcdir)/Indexable.cs          		\
	$(srcdir)/IndexingService.cs			\
	$(srcdir)/IndexingStatusResponse.cs		\
	$(srcdir)/InformationalMessagesRequest.cs	\
	$(srcdir)/Message.cs				\
	$(srcdir)/Property.cs           		\
	$(srcdir)/Query.cs              		\
	$(srcdir)/QueryPart.cs				\
	$(srcdir)/QueryResponses.cs            		\
	$(srcdir)/QueryableStatus.cs			\
	$(srcdir)/RemoteControl.cs			\
	$(srcdir)/RemovableIndexControl.cs		\
	$(srcdir)/Snippet.cs				\
	$(srcdir)/Transport.cs				\
	$(srcdir)/UnixTransport.cs			\
	$(srcdir)/Versioned.cs

LOCAL_ASSEMBLIES =					\
	../Util/Util.dll

ASSEMBLIES = 						\
	$(BEAGLED_LIBS)                 		\
	$(SHARPZIPLIBS_LIBS)				\
	-r:Mono.Posix					\
	$(LOCAL_ASSEMBLIES:%=-r:%)

RESOURCES =

$(TARGET): $(CSFILES) $(LOCAL_ASSEMBLIES) $(RESOURCES)
	$(CSC) -out:$@ $(CSFLAGS) $(CSFILES) $(ASSEMBLIES) $(RESOURCES)

all: $(TARGET)

install-data-local: $(TARGET)
	$(mkinstalldirs) $(DESTDIR)$(pkglibdir)
	$(INSTALL_DATA) $(TARGET) $(TARGET).mdb $(DESTDIR)$(pkglibdir)

uninstall-local:
	rm -f $(DESTDIR)$(pkglibdir)/$(TARGET) $(DESTDIR)$(pkglibdir)/$(TARGET).mdb

EXTRA_DIST =		\
	$(CSFILES)

CLEANFILES =		\
	$(TARGET)	\
	$(TARGET).mdb
