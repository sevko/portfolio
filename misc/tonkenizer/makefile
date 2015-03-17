.PHONY: all clean

all:
	gcc tokenizer.c -Og -o tokenizer -std=c99 -lpcre

run: all
	./tokenizer

clean:
	rm tokenizer
