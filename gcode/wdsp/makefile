MAIN_PROGRAM=wdsp

CFILES=$(shell ls *.c)
CPPFILES=$(shell ls *.cpp)

COBJECTS=$(CFILES:%.c=%.o)
CPPOBJECTS=$(CPPFILES:%.cpp=%.o)

CFLAGS= -g -O0
CPPFLAGS=$(CFLAGS)	

.PHONY: all
all: $(MAIN_PROGRAM)

.PHONY: clean
clean: 
	rm *.o $(MAIN_PROGRAM).exe -rf

$(COBJECTS):%.o:%.c
	gcc -c $(CFLAGS)  -o $@ $<

$(CPPOBJECTS):%.o:%.cpp
	g++ -c $(CPPFLAGS) -o $@ $<

$(MAIN_PROGRAM):$(COBJECTS) $(CPPOBJECTS)
	g++ -o $@ $^ 
