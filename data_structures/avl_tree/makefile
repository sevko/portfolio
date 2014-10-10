PROJECT_EXECUTABLE = bin/avl_tree
FLAGS = -Wall -Wextra -std=c99 \
	-I ./ \
	-I ../binary_tree/src/ \
	-I ../singly_linked_list/src/ \
	-I ../queue/src/
C_COMPILER = gcc $(FLAGS)
CC = @echo "\tcc $@" && $(C_COMPILER)

SRC = $(wildcard src/*.c)
OBJ = $(patsubst %.c, bin/%.o, $(foreach file, $(SRC), $(notdir $(file))))
OBJ += bin/queue.o bin/singly_linked_list.o bin/binary_tree.o

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

bin:
	@mkdir bin

bin/binary_tree.o: ../binary_tree/src/binary_tree.c
	$(CC) -o $@ -c $^

bin/queue.o: ../queue/src/queue.c
	$(CC) -o $@ -c $^

bin/singly_linked_list.o: ../singly_linked_list/src/singly_linked_list.c
	$(CC) -o $@ -c $^

clean:
	@rm -rf bin $(PROJECT_EXECUTABLE)
