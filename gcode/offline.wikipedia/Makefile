XMLBZ2 ?= $(shell readlink -f ./test/test.xml.bz2)
LANG ?= en
IDX := $(patsubst %.xml.bz2,%.idx,${XMLBZ2})
IDXDB := $(patsubst %.xml.bz2,%.db,${XMLBZ2})
ifeq (${XMLBZ2}, ${IDX})
$(error wikipeida xml.bz2 dump file must end with extension ".xml.bz2")
endif


.PHONY:	inform wikipedia

all:	inform

inform:
	@echo Make sure you have Python, Perl, PHP5, Xapian, Django installed
	@echo Also make sure you have cpan String::ShellQuote perl module
	@echo after that, run \'make wikipedia XMLBZ2="Path to your wikipedia dump download file"\' 
	@echo you can get the dump file with the following:
	@echo wget http://download.wikimedia.org/enwiki/latest/enwiki-latest-pages-articles.xml.bz2
	@echo if you do not specify the xmlbz2, then a testing file will be used, which contains only 1 article
	@echo and you must point your browser to \'http://localhost:8000/article/Andalusia\'
	@echo and also expect a unsatisfactory rendering of the page, since all templates are not parsed

wikipedia: ${IDXDB} idxdb

XMLBZ2: 
	@echo
	@bash -c 'if test -e "${XMLBZ2}; then exit 0; fi; \
		read -p "Oops, seems you do not have ${XMLBZ2}, do you want to download it (warning: take a lot of time, go to sleep!) y/N: " ans;\
		if test "$$ans" != Y -a "$$ans" != y; then echo stop.; false; fi'
	@wget http://download.wikimedia.org/enwiki/latest/enwiki-latest-pages-articles.xml.bz2 -O ${XMLBZ2}

IDX: ${IDX}
${IDX}: ${XMLBZ2}
	cd ./bz2-src/ && make
	./mparser.py ${XMLBZ2} > ${IDX}.2
	export LC_ALL=C && sort -k 10 ${IDX}.2 > $@
	rm ${IDX}.2
	wiki-beagrep ${IDX}


idxdb:
	mkdir -p ../../../external/bin/linux/ext/
	echo '${XMLBZ2}' > ../../../external/bin/linux/ext/wiki-$(LANG).txt
	ln -sf ../../../external/bin/linux/ext/wiki-$(LANG).txt $(LANG).py
${IDXDB}: ${IDX}
	@mkdir ./mediawiki_sa/templates/ ./mediawiki_sa/images_cache -p
	@echo Index built - we are done
	@echo To run your local wikipedia, just
	@echo
	@echo cd mywiki
	@echo python manage.py runserver
	@echo
	@echo ... and point your browser to http://localhost:8000/
