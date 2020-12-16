#include <iostream>
#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <sys/socket.h>
#include <netdb.h>

#define BUF_SIZE 500

using namespace std;

int main(int argc, char *argv[]) {
  struct addrinfo hints, *res = nullptr;
  int errcode;
  char addrstr[100];
  void *ptr;

  memset (&hints, 0, sizeof (hints));
  hints.ai_family = PF_UNSPEC;
  hints.ai_socktype = SOCK_STREAM;
  hints.ai_flags |= AI_CANONNAME;
  const char* host = "google.com\0";
  errcode = getaddrinfo (host, NULL, &hints, &res);
  cout << errcode << endl;
  cout << gai_strerror(errno) << endl;

  if (errcode != 0) {
    // ERROR
  } else {
    int size = 14;
    for (auto p = res; p != NULL; p = p->ai_next) {
        for (int i = 0; i < 14; i++) {
            cout << +static_cast<unsigned char>(p->ai_addr->sa_data[i]) << ".";
        }
        cout << endl;
    }
  }

  freeaddrinfo(res);
}

