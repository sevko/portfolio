PROJECT_EXECUTABLE = bin/des
FLAGS = -Wall -Wextra -I ./ -std=c99 -I ../bit_ops/src/
LIBS = -ltap
C_COMPILER = gcc $(FLAGS)
CC = @echo "\tcc $@" && $(C_COMPILER)

SRC = $(wildcard src/*.c)
OBJ = $(patsubst %.c, bin/%.o, $(foreach file, $(SRC), $(notdir $(file))))
OBJ += bin/bit_ops.o

.PHONY: all debug run clean

test: FLAGS += -O0 -g3 -DDES_TEST
test: bin $(PROJECT_EXECUTABLE)

all: FLAGS += -Ofast
all: bin $(PROJECT_EXECUTABLE)

run: test
	./$(PROJECT_EXECUTABLE)

$(PROJECT_EXECUTABLE): $(OBJ)
	$(CC) -o $@ $^ $(LIBS)

bin/%.o: src/%.c
	$(CC) -o $@ -c $^

bin/bit_ops.o: ../bit_ops/src/bit_ops.c
	$(CC) -o $@ -c $^

bin:
	@mkdir bin

clean:
	@rm -rf bin $(PROJECT_EXECUTABLE)
