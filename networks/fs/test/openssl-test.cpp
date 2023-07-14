#ifndef FS_CRYPTO_H
#define FS_CRYPTO_H

#include <cstdint>
#include <vector>

#include <iostream>
#include <string>
#include <cstdio>
#include <cstring>
#include <openssl/sha.h>

int main()
{
    using namespace std;
    unsigned char digest[SHA256_DIGEST_LENGTH];

    cout << "Input string to make hash" << endl;
    std::string str;
    getline(cin, str);

    SHA256((unsigned char*)str.c_str(), str.length(), (unsigned char*)&digest);    

    char mdString[SHA256_DIGEST_LENGTH*2+1];

    for(int i = 0; i < SHA256_DIGEST_LENGTH; i++)
        sprintf(&mdString[i*2], "%02x", (unsigned int)digest[i]);

    cout << "Text : " << str << endl;
    printf("SHA256 digest: %s\n", mdString);

    return 0;
}

#endif
