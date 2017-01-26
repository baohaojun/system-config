INCLUDE_PATH    := ./src
SOURCE_PATH     := ./src
DEPENDENCY_PATH := ./src/dep
OBJECT_PATH     := ./src/obj

PROGRAM_NAME    := clang-complete
LLVM_CONFIG     := llvm-config-3.8

LDLIBS := $(shell $(LLVM_CONFIG) --ldflags) -lclang
CFLAGS += -std=c99 $(shell $(LLVM_CONFIG) --cflags) -Wall -Wextra -pedantic -O3


include makefile.mk
