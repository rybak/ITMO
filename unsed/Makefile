objects = readlines.o readlines-main.o

readlines-main: $(objects)
	gcc -o readlines-main $(objects)

readlines.o : readlines.c readlines.h
	gcc -c readlines.c

readlines-main.o : readlines-main.c
	gcc -c readlines-main.c

clean:
	rm readlines-main $(objects)
