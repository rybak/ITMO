include ../common/common.mk

build : chat nchat

rebuild : clean build

clean :
	$(RM) *.o
	$(RM) chat
	$(RM) nchat

chat : chat.cpp common.o kbhit.o ra.o ports.h user.o
	$(CC) chat.cpp\
		kbhit.o common.o ma.o\
		ra.o user.o chat_message.o\
		receiver.o little_receiver.o tcp_socket.o\
		announcer.o announce_message.o\
		-o chat

nchat : n.cpp common.o kbhit.o sl.o ports.h user.o
	$(CC) n.cpp\
		kbhit.o common.o ma.o\
		sl.o user.o chat_message.o\
		sender.o tcp_socket.o\
		listener.o announce_message.o\
		-o nchat

ra.o : ra.cpp ra.h receiver.o announcer.o announce_message.o chat.h common.o ma.o user.o
	$(CCO) ra.cpp

sl.o : sl.cpp sl.h sender.o listener.o announce_message.o chat.h common.o ma.o user.o
	$(CCO) sl.cpp

announcer.o : announcer.cpp announcer.h announce_message.o ports.h chat.h common.o
	$(CCO) announcer.cpp

sender.o : sender.cpp sender.h ma.o chat_message.o tcp_socket.o
	$(CCO) sender.cpp

receiver.o : receiver.cpp receiver.h ma.o chat_message.o tcp_socket.o little_receiver.o
	$(CCO) receiver.cpp

little_receiver.o : little_receiver.cpp little_receiver.h
	$(CCO) little_receiver.cpp

listener.o : listener.cpp listener.h ports.h common.o ma.o announce_message.o
	$(CCO) listener.cpp

# messages

announce_message.o : announce_message.cpp announce_message.h ma.o common.o
	$(CCO) announce_message.cpp

chat_message.o : chat_message.cpp chat_message.h common.o
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

tcp_socket.o : tcp_socket.cpp tcp_socket.h common.o
	$(CCO) tcp_socket.cpp
