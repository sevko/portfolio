FLAGS = -std=c99 -Wall
EXECUTABLE = example

release: FLAGS += -Ofast
dev: FLAGS += -Og -g

release: bin
dev: bin

bin: example.re
	re2c -o example.c example.re
	gcc -o $(EXECUTABLE) $(FLAGS) example.c

run: bin
	./$(EXECUTABLE)

clean:
	rm example.c $(EXECUTABLE)
