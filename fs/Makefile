INCLUDE=../common
CF=$(INCLUDE)/common

CC=g++ -std=c++11 -O2 -I"$(INCLUDE)" -Wall
LIB_CRYPTO=/usr/lib/libcrypto++.a
LIB_OPENSSL=-lcrypto 
CCO=$(CC) -c
RM=rm -f

main : main.cpp ports.h
	$(CC) main.cpp -o main $(LIB_CRYPTO)

clean :
	$(RM) *.o
	$(RM) main
	$(RM) a.out

rebuild : clean main
