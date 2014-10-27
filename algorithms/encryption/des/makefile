PROJECT_EXECUTABLE = bin/des
FLAGS = -Wall -Wextra -I ./ -std=c99 -I ../bit_ops/src/
C_COMPILER = gcc $(FLAGS)
CC = @echo "\tcc $@" && $(C_COMPILER)

SRC = $(wildcard src/*.c)
OBJ = $(patsubst %.c, bin/%.o, $(foreach file, $(SRC), $(notdir $(file))))
OBJ += bin/bit_ops.o

.PHONY: all debug run clean

all: FLAGS += -Ofast
all: bin $(PROJECT_EXECUTABLE)

debug: FLAGS += -O0 -g3
debug: bin $(PROJECT_EXECUTABLE)

run: all
	./$(PROJECT_EXECUTABLE)

$(PROJECT_EXECUTABLE): $(OBJ)
	$(CC) -o $@ $^

bin/%.o: src/%.c
	$(CC) -o $@ -c $^

bin/bit_ops.o: ../bit_ops/src/bit_ops.c
	$(CC) -o $@ -c $^

bin:
	@mkdir bin

clean:
	@rm -rf bin $(PROJECT_EXECUTABLE)
