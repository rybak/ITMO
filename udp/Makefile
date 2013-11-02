INCLUDE=../common
CC=g++ -std=c++11 -O2 -I"$(INCLUDE)"
CS=$(INCLUDE)/common
S=$(CS).cpp
COMMON=$(CS).h $(S)

all: receiver sender

receiver: receiver.cpp $(COMMON)
	$(CC) receiver.cpp $(S) -o receiver

sender: sender.cpp $(COMMON)
	$(CC) sender.cpp $(S) -o sender

