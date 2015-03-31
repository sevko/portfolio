PROJECT_EXECUTABLE = bin/parser
FLAGS = -Wall -Wextra -I ./ -std=c99
C_COMPILER = gcc $(FLAGS)
CC = @echo "\tcc $@" && $(C_COMPILER)
LIBS = $(foreach lib, fl, -l$(lib))

SRC = $(wildcard src/*.c)
OBJ = $(foreach file, flex bison, bin/$(file).o)

.PHONY: all debug run clean

all: FLAGS += -Ofast
all: bin $(PROJECT_EXECUTABLE)

debug: FLAGS += -Og -g3
debug: bin $(PROJECT_EXECUTABLE)

run: all
	./$(PROJECT_EXECUTABLE)

$(PROJECT_EXECUTABLE): $(OBJ)
	$(CC) -o $@ $^ $(LIBS)

bin/flex.o: bin/flex.c bin/bison.c
	$(CC) -w -o $@ -c bin/flex.c

bin/flex.c: src/flex.l
	flex -o $@ $^

bin/bison.o: bin/bison.c
	$(CC) -w -o $@ -c $^

bin/bison.c: src/bison.y
	bison -d $^ -o $@

bin:
	@mkdir bin

clean:
	@rm -rf bin $(PROJECT_EXECUTABLE)
