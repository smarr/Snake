#!/usr/bin/env make

CC		=gcc
CFLAGS		=-Wno-endif-labels -std=gnu99 $(DBG_FLAGS) $(INCLUDES)
LDFLAGS		=

INSTALL		=install

CSOM_LIBS	=-ldl
CORE_LIBS	=-lm



############ global stuff -- overridden by ../Makefile

ROOT_DIR	?= $(PWD)/../../..
SRC_DIR		?= $(PWD)/../../src

ST_DIR		?= $(ROOT_DIR)/Smalltalk
EX_DIR		?= $(ROOT_DIR)/Examples
TEST_DIR	?= $(ROOT_DIR)/TestSuite

############# include path

INCLUDES		=-I$(SRC_DIR)


#
#
#
#  metarules
#

.SUFFIXES: .pic.o

.PHONY: clean clobber test

all: Terminal


debug : DBG_FLAGS=-DDEBUG -g
debug: all

profiling : DBG_FLAGS=-g -pg
profiling : LDFLAGS+=-pg
profiling: all


.c.pic.o:
	$(CC) $(CFLAGS) -fPIC -g -c $< -o $*.pic.o


clean:
	rm -Rf $(CLEAN)

Terminal:
	$(CC) $(CFLAGS) `$(SRC_DIR)/ostool.exe l Terminal.som`\
		-o `$(SRC_DIR)/ostool.exe s Terminal.som` \
		Terminal.c

