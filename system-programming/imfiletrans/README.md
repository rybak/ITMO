Start up server

    ./server localhost 1234

Then send files to server

    ./ms.sh filename.txt

Sender will output the token, which can be used to receive the file.

To receive the file in the out.txt

    ./mr.sh <TOKEN>
