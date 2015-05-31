EXECUTABLE_PATH = bin/sieve_of_eratosthenes

all: bin $(EXECUTABLE_PATH)

run: all
	bin/sieve_of_eratosthenes

# `ghc` builds its own dependency tree and only recompiles when it detects
# changes, so we simply force this recipe to run whenever `make` is invoked and
# delegate all of the build logic to `ghc`.
FORCE:
$(EXECUTABLE_PATH): FORCE
	ghc -Wall -fno-warn-type-defaults \
		-hidir $(BIN_DIR) -odir $(BIN_DIR) \
		-o $@ --make Test.hs

bin:
	mkdir bin

clean:
	if [ -d bin ]; then rm -rf bin; fi
