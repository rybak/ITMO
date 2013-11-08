INCLUDE=../common
CC=g++ -std=c++11 -O2 -I"$(INCLUDE)"
CS=$(INCLUDE)/common
S=$(CS).cpp
COMMON=$(CS).h $(S)
message=message.cpp message.h chat.h $(COMMON)
announcer=announcer.cpp announcer.h chat.h $(message) $(COMMON)
ma=ma.cpp ma.h
kbhit=kbhit.cpp kbhit.h
chatter=chatter.h chatter.cpp sender.cpp sender.h receiver.cpp receiver.h listener.cpp listener.h ports.h

chat: chat.cpp chat.h $(COMMON) $(announcer) $(message) $(ma) $(kbhit) $(chatter)
	$(CC) kbhit.cpp ma.cpp announcer.cpp message.cpp chat.cpp chatter.cpp sender.cpp receiver.cpp listener.cpp $(S) -o chat

test: test.cpp $(COMMON)
	$(CC) test.cpp $(S)
