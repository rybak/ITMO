INCLUDE=../common
CC=g++ -std=c++11 -O2 -I"$(INCLUDE)"
CCO=$(CC) -g -c
CF=$(INCLUDE)/common

chat: chat.cpp common announcer.o kbhit.o chatter.o ports.h user.o
	$(CC) chat.cpp kbhit.o chatter.o ma.cpp announcer.o message.o sender.o listener.o receiver.o user.o $(CF).o -o chat

kbhit.o : kbhit.cpp kbhit.h
	$(CCO) kbhit.cpp

chatter.o : chatter.cpp chatter.h chat.h common ma.o message.o sender.o receiver.o announcer.o listener.o user.o
	$(CCO) chatter.cpp

message.o : message.cpp message.h
	$(CCO) message.cpp

announcer.o : announcer.cpp announcer.h message.o ports.h chat.h common
	$(CCO) announcer.cpp

common : $(CF).cpp $(CF).h
	$(CCO) $(CF).cpp

ma.o : ma.cpp ma.h
	$(CCO) ma.cpp

sender.o : sender.cpp sender.h message.o
	$(CCO) sender.cpp

receiver.o : receiver.cpp receiver.h ma.o
	$(CCO) receiver.cpp

listener.o : listener.cpp listener.h ports.h common ma.o message.o 
	$(CCO) listener.cpp

user.o : user.cpp user.h message.o
	$(CCO) user.cpp

