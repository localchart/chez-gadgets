#include <WinSock2.h>
#include <string.h>
#include "scheme.h"
//32 bit only

int tcp_startup()
{
    WSADATA data;
    return WSAStartup(MAKEWORD(2, 2), &data);
}

int tcp_socket()
{
    return socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
}

int tcp_connect(int sock, const char* ip, int port)
{
    struct sockaddr_in addr;
    memset(&addr, 0, sizeof(addr));

    addr.sin_family = AF_INET;
    addr.sin_addr.S_un.S_addr = inet_addr(ip);
    addr.sin_port = htons(port);

    Sdeactivate_thread();
    int ret = connect(sock, (struct sockaddr*)(&addr), sizeof(addr));
    Sactivate_thread();

    return ret;
}

int tcp_set_reuse_addr(int sock)
{
    int b = 1;
    return setsockopt(sock, SOL_SOCKET, SO_REUSEADDR, (char*)(&b), sizeof(b));
}

int tcp_bind(int sock, int port)
{
    struct sockaddr_in addr;
    memset(&addr, 0, sizeof(addr));

    addr.sin_family = AF_INET;
    addr.sin_addr.S_un.S_addr = INADDR_ANY;
    addr.sin_port = htons(port);

    return bind(sock, (struct sockaddr*)(&addr), sizeof(addr));
}

int tcp_listen(int sock, int backlog)
{
    return listen(sock, backlog);
}

int tcp_accept(int sock)
{
    struct sockaddr_in addr;
    int len = sizeof(addr);
    Sdeactivate_thread();
    int s = accept(sock, (struct sockaddr*)(&addr), &len);
    Sactivate_thread();
    return s;
}

int tcp_send(int sock, ptr bytes, size_t offset, size_t len)
{
    Slock_object(bytes);
    Sdeactivate_thread();
    int ret = send(sock, (char*)(Sbytevector_data(bytes)) + offset, len, 0);
    Sactivate_thread();
    Sunlock_object(bytes);

    return ret;
}

int tcp_recv(int sock, ptr bytes, size_t offset, size_t len)
{
    Slock_object(bytes);
    Sdeactivate_thread();
    int ret = recv(sock, (char*)(Sbytevector_data(bytes)) + offset, len, 0);
    Sactivate_thread();
    Sunlock_object(bytes);

    return ret;
}

int tcp_set_recv_timeout(int sock, int ms)
{
    return setsockopt(sock, SOL_SOCKET, SO_RCVTIMEO, (char*)(&ms), sizeof(ms));
}

int tcp_set_send_timeout(int sock, int ms)
{
    return setsockopt(sock, SOL_SOCKET, SO_SNDTIMEO, (char*)(&ms), sizeof(ms));
}

int tcp_shutdown(int sock)
{
    return shutdown(sock, SD_BOTH);
}

int tcp_close(int sock)
{
    return closesocket(sock);
}
