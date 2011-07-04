all: blocksort.c huffman.c crctable.c randtable.c bzlibfill.c
	gcc -DHAVE_LARGEFILE_SUPPORT -fpic bz2module.c -shared -o bz2.so  -I/usr/include/python2.6/ decompress.c blocksort.c huffman.c crctable.c randtable.c new.c bzlibfill.c
	cp -f bz2.so ../mod
	cp -f bz2.so ../mod64
clean:
	rm -f bz2.so decompress.o blocksort.o huffman.o crctable.o randtable.o new.o bzlibfill.o
