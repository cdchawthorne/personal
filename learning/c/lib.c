#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <errno.h>
#include <string.h>
#include <assert.h>

int main() {
    int err;

    struct addrinfo * info;
    if (getaddrinfo("trellis1.tug-libraries.on.ca", "www", NULL, &info)) {
        printf("Get info failed\n");
        exit(1);
    }

    int tcp_sock = socket(info->ai_family, info->ai_socktype, info->ai_protocol);
    if (tcp_sock == -1) {
        printf("Socket creation failed...\n");
        exit(1);
    }

    if (connect(tcp_sock, info->ai_addr, info->ai_addrlen)) {
        printf("Connection failed...\n");
        exit(1);
    }

    char msg[] =
"POST /cgi-bin/Pwebrecon.cgi HTTP/1.1\r\n\
HOST: trellis1.tug-libraries.on.ca\r\n\
Content-Type: application/x-www-form-urlencoded\r\n\
Content-Length: 95\r\n\r\n\
PAGE=pbPatron&\
BC=21187004642336&\
LN=Hawthorne&\
PID=erRsJ2_ABG90EZXH4ODey4fpur3&\
SEQ=20121211183333\
\r\n\
\r\n";

    if (send(tcp_sock, (void*)(msg), sizeof(msg), 0) != sizeof(msg)) {
        printf("GET request failed\n");
        printf("%d\n", errno);
        strerror(errno);
        exit(1);
    }

    char x;

    while (1) {
        err = recv(tcp_sock, &x, 1, 0);
        if (err < 0) {
            printf("Receive failed\n");
            exit(1);
        } else if (err == 1) {
            putchar(x);
        } else {
            assert(err == 0);
            break;
        }
    }

    if (close(tcp_sock)) {
        printf("Close failed\n");
        exit(1);
    }

    exit(0);
}
