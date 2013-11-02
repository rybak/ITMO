C=g++ -std=c++11 -O2 -I "../include"
S=common.cpp
COMMON=common.h $(S)

all: receiver sender

receiver: receiver.cpp $(COMMON)
	$(CC) receiver.cpp $(S) -o receiver

sender: sender.cpp $(COMMON)
	$(CC) sender.cpp $(S) -o sender
C
