PREFIX?=/usr/local
DESTDIR?=
INSTALL?=install

OSFILES = $(filter-out $(wildcard *~),$(wildcard os.*))
SCRIPTS = $(filter-out $(wildcard *~),$(wildcard guilt-*))

.PHONY: all 
all:
	@echo "Nothing to build, it is all bash :)"
	@echo "Try make install"

.PHONY: install
install:
	$(INSTALL) -d $(DESTDIR)$(PREFIX)/bin/
	$(INSTALL) -m 755 guilt $(DESTDIR)$(PREFIX)/bin/
	$(INSTALL) -d $(DESTDIR)$(PREFIX)/lib/guilt/
	$(INSTALL) -m 755 $(SCRIPTS) $(DESTDIR)$(PREFIX)/lib/guilt/
	$(INSTALL) -m 644 $(OSFILES) $(DESTDIR)$(PREFIX)/lib/guilt/

.PHONY: uninstall
uninstall:
	./uninstall $(DESTDIR)$(PREFIX)/bin/ $(SCRIPTS)

.PHONY: doc
doc:
	$(MAKE) -C Documentation all

.PHONY: install-doc
install-doc:
	$(MAKE) -C Documentation install PREFIX=$(PREFIX) DESTDIR=$(DESTDIR) INSTALL=$(INSTALL)

.PHONY: test
test:
	make -C regression all

.PHONY: clean
clean: 
	$(MAKE) -C Documentation clean 
