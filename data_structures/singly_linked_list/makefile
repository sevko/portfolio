PROJECT_NAME = singly_linked_list
FLAGS = -Wall -Wextra -I ./
CC = gcc $(FLAGS)

SRC = $(wildcard src/*.c)
OBJ = $(patsubst %.c, bin/%.o, $(foreach file, $(SRC), $(notdir $(file))))

.PHONY: all run clean

all: bin bin/$(PROJECT_NAME)

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
