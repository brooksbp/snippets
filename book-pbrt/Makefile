# Makefile

MARCH=-m64

OPT=-g3
OPT += -std=c++11

DEFS=-DDEBUG

ARCH = $(shell uname)

CC=gcc
CXX=clang++
LD=$(CXX) $(OPT) $(MARCH)
INCLUDE=-I. -Icore
WARN=-Wall
CXXFLAGS=$(OPT) $(MARCH) $(INCLUDE) $(WARN) $(DEFS)
CCFLAGS=$(CXXFLAGS)
LIBS=-lm

LIBSRS=$(wildcard core/*.cpp)

LIBOBJS=$(addprefix objs/, $(subst /,_,$(LIBSRS:.cpp=.o)))

HEADERS = $(wildcard */*.h)

default: dirs bin/pbrt

dirs:
	/bin/mkdir -p bin objs

.PHONY: dirs tools

$(LIBOBJS): $(HEADERS)

objs/libpbrt.a: $(LIBOBJS)
	@echo "Building $@"
	@ar rcs $@ $(LIBOBJS)

objs/core_%.o: core/%.cpp
	@echo "Building $@"
	@$(CXX) $(CXXFLAGS) -o $@ -c $<

objs/main_%.o: main/%.cpp
	@echo "Building $@"
	@$(CXX) $(CXXFLAGS) -o $@ -c $<

bin/pbrt: objs/main_pbrt.o objs/libpbrt.a
	@echo "Linking $@"
	@$(CXX) $(CXXFLAGS) -o $@ $^ $(LIBS)

clean:
	rm -f objs/* bin/*
