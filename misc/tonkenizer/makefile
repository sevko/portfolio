.PHONY: all clean

FLAGS = -Wall -std=c99 -lpcre
debug: FLAGS += -Og -g3
all: FLAGS += -Ofast

all: bin
debug: bin

bin: tokenizer.c
	gcc tokenizer.c -o tokenizer $(FLAGS)

run: bin
	./tokenizer

clean:
	rm tokenizer
