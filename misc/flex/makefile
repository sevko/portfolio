FLAGS = -std=c99 -Wall
EXECUTABLE = bin/tokenizer

.PHONY: bin

release: FLAGS += -Ofast
dev: FLAGS += -Og -g

release: bin
dev: bin

bin: tokenizer.l
	[ -d bin ] || mkdir bin
	flex -o bin/tokenizer.c tokenizer.l
	gcc -o $(EXECUTABLE) $(FLAGS) bin/tokenizer.c -lfl

run: bin
	cat corpus.txt | ./$(EXECUTABLE)

clean:
	rm -rf bin
