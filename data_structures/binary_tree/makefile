PROJECT_EXECUTABLE = bin/binary_tree
FLAGS = -Wall -Wextra -I ./ -std=c99
LIBS = -ltap
C_COMPILER = gcc $(FLAGS)
CC = @echo "\tcc $@" && $(C_COMPILER)

SRC = $(wildcard src/*.c)
OBJ = $(patsubst %.c, bin/%.o, $(foreach file, $(SRC), $(notdir $(file))))

.PHONY: all debug run clean

all: FLAGS += -Ofast
all: bin $(PROJECT_EXECUTABLE)

debug: FLAGS += -O0 -g3
debug: bin $(PROJECT_EXECUTABLE)

run: all
	./$(PROJECT_EXECUTABLE) | tap-spec

$(PROJECT_EXECUTABLE): $(OBJ)
	$(CC) -o $@ $^ $(LIBS)

bin/%.o: src/%.c
	$(CC) -o $@ -c $^

bin:
	@mkdir bin

clean:
	@rm -rf bin $(PROJECT_EXECUTABLE)
