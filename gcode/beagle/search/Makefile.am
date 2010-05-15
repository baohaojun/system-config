SUBDIRS = icons

CSC = $(MCS) -debug
CSFLAGS = -target:exe

ASSEMBLY_NAME = Beagle.Search
ASSEMBLY = $(ASSEMBLY_NAME).exe

if ENABLE_DESKTOP_LAUNCH
CSFLAGS += -define:ENABLE_DESKTOP_LAUNCH
endif

if ENABLE_XDG_OPEN
CSFLAGS += -define:ENABLE_XDG_OPEN
endif

if ENABLE_GALAGO
CSFLAGS += -define:ENABLE_GALAGO
endif

if ENABLE_THUNDERBIRD
CSFLAGS += -define:ENABLE_THUNDERBIRD
endif

if ENABLE_AVAHI
CSFLAGS += -define:ENABLE_AVAHI
endif

if ENABLE_GOOGLEBACKENDS
CSFLAGS += -define:ENABLE_GOOGLEBACKENDS
endif

SOURCES =							\
	$(srcdir)/AssemblyInfo.cs				\
	$(srcdir)/Beagle.Search.Pages/Base.cs			\
	$(srcdir)/Beagle.Search.Pages/IndexInfo.cs		\
	$(srcdir)/Beagle.Search.Pages/NoMatch.cs		\
	$(srcdir)/Beagle.Search.Pages/QuickTips.cs		\
	$(srcdir)/Beagle.Search.Pages/RootUser.cs		\
	$(srcdir)/Beagle.Search.Pages/StartDaemon.cs		\
	$(srcdir)/Beagle.Search.Tiles/ActionMenuItem.cs		\
	$(srcdir)/Beagle.Search.Tiles/Application.cs		\
	$(srcdir)/Beagle.Search.Tiles/ArchivedFile.cs		\
	$(srcdir)/Beagle.Search.Tiles/Audio.cs			\
	$(srcdir)/Beagle.Search.Tiles/CApplet.cs		\
	$(srcdir)/Beagle.Search.Tiles/Calendar.cs		\
	$(srcdir)/Beagle.Search.Tiles/Contact.cs		\
	$(srcdir)/Beagle.Search.Tiles/Docbook.cs		\
	$(srcdir)/Beagle.Search.Tiles/File.cs			\
	$(srcdir)/Beagle.Search.Tiles/Folder.cs			\
	$(srcdir)/Beagle.Search.Tiles/HitFlavor.cs		\
	$(srcdir)/Beagle.Search.Tiles/IMLog.cs			\
	$(srcdir)/Beagle.Search.Tiles/Image.cs			\
	$(srcdir)/Beagle.Search.Tiles/MailAttachment.cs		\
	$(srcdir)/Beagle.Search.Tiles/MailMessage.cs		\
	$(srcdir)/Beagle.Search.Tiles/Manpage.cs		\
	$(srcdir)/Beagle.Search.Tiles/Note.cs			\
	$(srcdir)/Beagle.Search.Tiles/OpenWithMenu.cs		\
	$(srcdir)/Beagle.Search.Tiles/Presentation.cs		\
	$(srcdir)/Beagle.Search.Tiles/RSSFeed.cs		\
	$(srcdir)/Beagle.Search.Tiles/Spreadsheet.cs		\
	$(srcdir)/Beagle.Search.Tiles/Task.cs			\
	$(srcdir)/Beagle.Search.Tiles/TextDocument.cs		\
	$(srcdir)/Beagle.Search.Tiles/ThumbnailFactory.cs	\
	$(srcdir)/Beagle.Search.Tiles/Tile.cs			\
	$(srcdir)/Beagle.Search.Tiles/TileAction.cs		\
	$(srcdir)/Beagle.Search.Tiles/TileActivator.cs		\
	$(srcdir)/Beagle.Search.Tiles/TileFlat.cs		\
	$(srcdir)/Beagle.Search.Tiles/TileGroup.cs		\
	$(srcdir)/Beagle.Search.Tiles/TileTemplate.cs		\
	$(srcdir)/Beagle.Search.Tiles/Utils.cs			\
	$(srcdir)/Beagle.Search.Tiles/Video.cs			\
	$(srcdir)/Beagle.Search.Tiles/WebHistory.cs		\
	$(srcdir)/Beagle.Search.Tray/NotificationArea.cs	\
	$(srcdir)/Beagle.Search.Tray/TrayIcon.cs		\
	$(srcdir)/Beagle.Search/CairoFu.cs			\
	$(srcdir)/Beagle.Search/Category.cs			\
	$(srcdir)/Beagle.Search/DetailsPane.cs			\
	$(srcdir)/Beagle.Search/Driver.cs			\
	$(srcdir)/Beagle.Search/Entry.cs			\
	$(srcdir)/Beagle.Search/GroupView.cs			\
	$(srcdir)/Beagle.Search/ISearch.cs			\
	$(srcdir)/Beagle.Search/ListCategory.cs			\
	$(srcdir)/Beagle.Search/NotificationArea.cs		\
	$(srcdir)/Beagle.Search/Panes.cs			\
	$(srcdir)/Beagle.Search/Search.cs			\
	$(srcdir)/Beagle.Search/SearchWindow.cs			\
	$(srcdir)/Beagle.Search/SortedTileList.cs		\
	$(srcdir)/Beagle.Search/Spinner.cs			\
	$(srcdir)/Beagle.Search/TileCategory.cs			\
	$(srcdir)/Beagle.Search/UIManager.cs			\
	$(srcdir)/Beagle.Search/WidgetFu.cs

LOCAL_ASSEMBLIES =			\
	../Util/Util.dll		\
	../Util/UiUtil.dll		\
	../BeagleClient/Beagle.dll

ASSEMBLIES =				\
	$(BEAGLE_UI_LIBS)		\
	$(LOCAL_ASSEMBLIES:%=-r:%)	\
	$(NDESK_DBUS_LIBS)		\
	$(NDESK_DBUS_GLIB_LIBS)		\
	-r:Mono.Posix			\
	-r:Mono.Cairo

pkglib_DATA = $(ASSEMBLY) $(ASSEMBLY).mdb $(ASSEMBLY).config

BIN_WRAPPERS = beagle-search

$(ASSEMBLY): $(SOURCES) $(LOCAL_ASSEMBLIES)
	$(CSC) -out:$@ $(CSFLAGS) $(SOURCES) $(ASSEMBLIES)

$(ASSEMBLY).mdb: $(ASSEMBLY)

beagle-search: beagle-search.in
	sed 					\
	-e "s:@pkglibdir@:${pkglibdir}:" 	\
	-e "s:@bash@:$(BASH):"			\
	< $(srcdir)/beagle-search.in > beagle-search
	chmod a+x beagle-search

@INTLTOOL_DESKTOP_RULE@

desktop_in_files = beagle-search.desktop.in
desktop_files = $(desktop_in_files:.desktop.in=.desktop)
desktop_h_files = $(desktop_in_files:.in=.in.h)

desktopdir       = $(datadir)/applications
desktop_DATA     = $(desktop_files)

autostartdir = $(sysconfdir)/xdg/autostart
autostart_DATA = beagle-search-autostart.desktop

man_MANS = beagle-search.1

all: $(BIN_WRAPPERS)

install-data-local: $(BIN_WRAPPERS)
	$(mkinstalldirs) $(DESTDIR)$(bindir)

	@sed -e "s|\#installed=1|installed=1|" < beagle-search > beagle-search.tmp
	$(INSTALL_SCRIPT) beagle-search.tmp $(DESTDIR)$(bindir)/beagle-search
	@rm -f beagle-search.tmp

uninstall-local:
	rm -f $(DESTDIR)$(bindir)/beagle-search

EXTRA_DIST =			\
	$(SOURCES)		\
	$(man_MANS)		\
	$(desktop_DATA).in	\
	$(autostart_DATA)	\
	$(ASSEMBLY).config	\
	beagle-search.in	\
	beagle-search.desktop.in.in

CLEANFILES =			\
	$(ASSEMBLY)		\
	$(ASSEMBLY).mdb		\
	beagle-search		\
	$(desktop_DATA)		\
	$(desktop_h_files)
