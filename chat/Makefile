INCLUDE=../common
CC=g++ -std=c++11 -O2 -I"$(INCLUDE)"
CCO=$(CC) -g -c
CF=$(INCLUDE)/common

chat: chat.cpp common.o kbhit.o chatter.o ports.h user.o
	$(CC) chat.cpp kbhit.o chatter.o ma.cpp announcer.o announce_message.o sender.o listener.o receiver.o user.o common.o chat_message.o -o chat

chatter.o : chatter.cpp sender.o receiver.o announcer.o listener.o announce_message.o chatter.h chat.h common.o ma.o user.o
	$(CCO) chatter.cpp

announcer.o : announcer.cpp announcer.h announce_message.o ports.h chat.h common.o
	$(CCO) announcer.cpp

sender.o : sender.cpp sender.h ma.o chat_message.o
	$(CCO) sender.cpp

receiver.o : receiver.cpp receiver.h ma.o chat_message.o
	$(CCO) receiver.cpp

listener.o : listener.cpp listener.h ports.h common.o ma.o announce_message.o 
	$(CCO) listener.cpp

# messages

announce_message.o : announce_message.cpp announce_message.h ma.o common.o
	$(CCO) announce_message.cpp

chat_message.o : chat_message.cpp chat_message.h
	$(CCO) chat_message.cpp

# common

user.o : user.cpp user.h announce_message.o
	$(CCO) user.cpp

common.o : $(CF).cpp $(CF).h
	$(CCO) $(CF).cpp

ma.o : ma.cpp ma.h
	$(CCO) ma.cpp

kbhit.o : kbhit.cpp kbhit.h
	$(CCO) kbhit.cpp

