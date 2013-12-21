#include <iostream>
#include <string>
#include <stdexcept>

using std::cout;
using std::endl;

#include "cryptopp/integer.h"
#include "cryptopp/md5.h"
using CryptoPP::Integer;
#include <cryptopp/sha.h>

// ~/lab/os-course/midterm/*.c <<< has shell-style sha calculation

int main( int argc, char* argv[] ) {
    CryptoPP::SHA256 sha256gen;
    byte hashb[256];
    const byte word[256] = "test";
    sha256gen.CalculateDigest(hashb, word, 4 );
    std::cout << (hashb[0] + 'A') << std::endl;
    return 0;
}

//int main( int, char** )
//{
//    CryptoPP::MD5 hash;
//    byte digest[ CryptoPP::MD5::DIGESTSIZE ];
//    std::string message = "abcdefghijklmnopqrstuvwxyz";
//
//    hash.CalculateDigest( digest, message.c_str(), message.length() );
//
//    CryptoPP::HexEncoder encoder;
//    std::string output;
//    encoder.Attach( new CryptoPP::StringSink( output ) );
//    encoder.Put( digest, sizeof(digest) );
//    encoder.MessageEnd();
//
//    std::cout << output << std::endl;
//
//    Integer i;
//    cout << "i: " << i << endl;
//    return 0;
//}
//
    //std::string password;
    //std::getline(std::cin, password);


