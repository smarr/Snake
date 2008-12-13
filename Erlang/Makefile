ERL_SRC = $(wildcard *.erl)
BEAM	= $(ERL_SRC:.erl=.beam)

all: $(BEAM)

test:
	./test.sh

%.beam: %.erl
	erlc $*.erl
