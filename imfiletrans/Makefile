

all : sender server receiver

sender : sender.cpp http.o
	g++ sender.cpp http.o -o sender

receiver : receiver.cpp http.o
	g++ receiver.cpp http.o -o receiver

http.o : http.cpp http.h
	g++ -g -c http.cpp

client.o : client.h client.cpp http.o
	g++ -g -std=c++11 -c client.cpp

server : server.cpp big_buffer.h client.o http.o
	g++ -g -std=c++11 server.cpp client.o http.o -o server
