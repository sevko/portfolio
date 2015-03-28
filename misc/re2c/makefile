FLAGS = -std=c99 -Wall
EXECUTABLE = bin/example

release: FLAGS += -Ofast
dev: FLAGS += -Og -g

release: bin
dev: bin

bin: example.re.c
	[ -d bin ] || mkdir bin
	re2c -o bin/example.c example.re.c
	gcc -o $(EXECUTABLE) $(FLAGS) bin/example.c

run: bin
	./$(EXECUTABLE)

clean:
	rm -rf bin
