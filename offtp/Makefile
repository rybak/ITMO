all : offtp receiver

receiver : receiver.cpp http.o
	g++ receiver.cpp http.o -o receiver

http.o : http.cpp http.h
	g++ -g -c http.cpp

offtp : offtp.cpp http.o
	g++ -g -std=c++11 offtp.cpp http.o -o offtp
