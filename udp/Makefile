INCLUDE=../common
CC=g++ -std=c++11 -O2 -I"$(INCLUDE)"
CCO=$(CC) -c
CS=$(INCLUDE)/common
S=$(CS).cpp
COMMON=$(CS).h $(S) message.h
RM=rm -f

all : receiver sender

rebuild : clean all

clean :
	$(RM) *.o
	$(RM) receiver
	$(RM) sender

receiver : receiver.cpp common.o udp.o
	$(CC) receiver.cpp common.o udp.o -o receiver

sender : sender.cpp common.o
	$(CC) sender.cpp common.o udp.o -o sender

udp.o : udp.h udp.cpp
	$(CC) -c udp.cpp -o udp.o

common.o : $(COMMON)
	$(CCO) $(S)
