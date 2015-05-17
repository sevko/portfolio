PROJECT_EXECUTABLE = bin/sieve_of_eratosthenes
FLAGS = -Wall -Wextra -I ./ -std=c99
C_COMPILER = gcc $(FLAGS)
CC = @echo "\tcc $@" && $(C_COMPILER)
LIBS = -lm

SRC = $(wildcard src/*.c)
OBJ = $(patsubst %.c, bin/%.o, $(foreach file, $(SRC), $(notdir $(file))))

.PHONY: all debug run clean

all: FLAGS += -Ofast
all: bin $(PROJECT_EXECUTABLE)

debug: FLAGS += -Og -g3
debug: bin $(PROJECT_EXECUTABLE)

run: all
	./$(PROJECT_EXECUTABLE)

$(PROJECT_EXECUTABLE): $(OBJ)
	$(CC) -o $@ $^ $(LIBS)

bin/%.o: src/%.c
	$(CC) -o $@ -c $^

bin:
	@mkdir bin

clean:
	@rm -rf bin $(PROJECT_EXECUTABLE)
