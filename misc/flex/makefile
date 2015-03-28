FLAGS = -std=c99 -Wall
EXECUTABLE = tokenizer

release: FLAGS += -Ofast
dev: FLAGS += -Og -g

release: bin
dev: bin

bin: tokenizer.l
	flex tokenizer.l
	gcc -o $(EXECUTABLE) $(FLAGS) lex.yy.c -lfl

run: bin
	cat corpus.txt | ./$(EXECUTABLE)

clean:
	rm tokenizer.c $(EXECUTABLE)
