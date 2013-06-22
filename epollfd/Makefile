CC=g++ -g -std=c++11

all : autofd.o epollfd.o

epollfd.o : epollfd.h epollfd.cpp
	$(CC) -c epollfd.cpp -o epollfd.o

autofd.o : autofd.cpp autofd.h
	$(CC) -c autofd.cpp -o autofd.o
