PROJECT_EXECUTABLE = bin/parser
FLAGS = -Wall -Wextra -I ./ -std=c99
C_COMPILER = gcc $(FLAGS)
CC = @echo "\tcc $@" && $(C_COMPILER)
LIBS = $(foreach lib, fl, -l$(lib))

SRC = $(wildcard src/*.c)
OBJ = $(foreach file, flex parser, bin/$(file).o)

.PHONY: all debug run clean

all: FLAGS += -Ofast
all: bin $(PROJECT_EXECUTABLE)

debug: FLAGS += -Og -g3
debug: bin $(PROJECT_EXECUTABLE)

run: all
	./$(PROJECT_EXECUTABLE)

$(PROJECT_EXECUTABLE): $(OBJ)
	$(CC) -o $@ $^ $(LIBS)

bin/flex.o: bin/flex.c
	$(CC) -w -o $@ -c $^

bin/flex.c: src/flex.l
	flex -o $@ $^

bin/parser.o: bin/flex.c src/parser.c
	$(CC) -o $@ -c src/parser.c

bin:
	@mkdir bin

clean:
	@rm -rf bin $(PROJECT_EXECUTABLE)
