include ../../common/common.mk

openssl : openssl-test.cpp
	$(CC) openssl-test.cpp -o openssl $(LIB_OPENSSL)

clean :
	$(RM) *.o
	$(RM) main
	$(RM) a.out

rebuild : clean openssl
