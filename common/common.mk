# directories
INCLUDE=../common
CF=$(INCLUDE)/common

# binaries
CC=g++ -std=c++11 -O2 -I"$(INCLUDE)" -Wall
CCO=$(CC) -c

# libs
LIB_CRYPTO=/usr/lib/libcrypto++.a
LIB_OPENSSL=-lcrypto 

# commands
RM=rm -f

