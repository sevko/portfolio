PROJECT_NAME = singly_linked_list
FLAGS = -Wall -Wextra -I ./
COMPILER = gcc $(FLAGS)
CC = @echo "cc $@" && $(COMPILER)

SRC = $(wildcard src/*.c)
OBJ = $(patsubst %.c, bin/%.o, $(foreach srcFile, $(SRC), $(notdir $(srcFile))))

.PHONY: all debug run clean

all: FLAGS += -Ofast
debug: FLAGS += -O0 -g3

all: bin bin/$(PROJECT_NAME)

debug: bin bin/$(PROJECT_NAME)

run: all
	bin/$(PROJECT_NAME)

bin/$(PROJECT_NAME): $(OBJ)
	$(CC) -o $@ $^

bin/%.o: src/%.c
	$(CC) -o $@ -c $^

bin:
	@mkdir bin

clean:
	@rm -rf bin
