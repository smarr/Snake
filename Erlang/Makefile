ERL_SRC = $(wildcard *.erl)
BEAM	= $(ERL_SRC:.erl=.beam)

all: $(BEAM)

test:
	./test.sh

clean:
	rm $(BEAM)

%.beam: %.erl
	erlc $*.erl
