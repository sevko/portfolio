EXECUTABLE_PATH = bin/sieve_of_eratosthenes
FLAGS = -Wall -fno-warn-type-defaults
CC = ghc

all: bin $(EXECUTABLE_PATH)

run: all
	bin/sieve_of_eratosthenes

$(EXECUTABLE_PATH): sieve_of_eratosthenes.hs test_sieve.hs
	$(CC) $(FLAGS) -hidir bin -odir bin -o $@ --make $^

bin:
	mkdir bin

clean:
	if [ -d bin ]; then rm -rf bin; fi
