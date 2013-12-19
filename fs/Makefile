INCLUDE=../common
CF=$(INCLUDE)/common

CC=g++ -std=c++11 -O2 -I"$(INCLUDE)" -Wall
CCO=$(CC) -c
RM=rm -f

main : main.cpp ports.h
	$(CC) main.cpp

clean :
	$(RM) *.o

rebuild : clean main
