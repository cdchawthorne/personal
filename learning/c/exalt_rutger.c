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
    if (getaddrinfo("wiki.spacesim.org", "www", NULL, &info)) {
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
"GET /index.php/Rutger_Theodoor_Ronald_Jansen_van_Doorn_Campbell HTTP/1.1\r\n\
HOST:wiki.spacesim.org\r\n\r\n";

    int i;
    for (i = 0; i < 1000; ++i) {
        printf("%d\n", i);
        if (send(tcp_sock, (void*)(msg), sizeof(msg), 0) != sizeof(msg)) {
            printf("GET request failed\n");
            printf("%d\n", errno);
            strerror(errno);
            exit(1);
        } else {
            printf("Succeeded!\n");
        }

        char x;

        while (1) {
            err = recv(tcp_sock, &x, 1, 0);
            if (err < 0) {
                printf("Receive failed\n");
                exit(1);
            } else if (err == 0) {
                break;
            }
        }
    }

    if (close(tcp_sock)) {
        printf("Close failed\n");
        exit(1);
    }

    exit(0);
}
