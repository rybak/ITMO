CC=g++ -g -std=c++11

all : autofd.o epollfd.o server

epollfd.o : epollfd.h epollfd.cpp
	$(CC) -c epollfd.cpp -o epollfd.o

autofd.o : autofd.cpp autofd.h
	$(CC) -c autofd.cpp -o autofd.o

http.o : http.h http.cpp
	$(CC) -c http.cpp -o http.o

server : http.o server.cpp epollfd.o
	$(CC) epollfd.o server.cpp http.o -o server
