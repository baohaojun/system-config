all:
	gmcs break.cs $(shell wlp Token.cs) $(shell wlp StandardTokenizerImpl.cs)
	cp *.exe ~/bin/$(shell uname|perl -npe 's/_.*//')/ext/`uname -m`/beagrep-break.exe

clean:
	git clean -fdx 
