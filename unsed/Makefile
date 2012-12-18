objects = readlines.o readlines-main.o
readlinesch = readlines.c readlines.h

readlines-main: $(objects)
	gcc -o readlines-main $(objects)

readlines.o : $(readlinesch)
	gcc -c readlines.c

readlines-main.o : readlines-main.c
	gcc -c readlines-main.c

clean:
	rm readlines-main $(objects)

all-shared: libreadlines.so readlines-main.c readlines.h
	gcc -L. -Wl,-rpath=. -o readlines-main-shared readlines-main.c -lreadlines

libreadlines.so : libreadlines.o
	gcc -shared -o libreadlines.so libreadlines.o

libreadlines.o : $(readlinesch)
	gcc -c -fPIC readlines.c -o libreadlines.o
