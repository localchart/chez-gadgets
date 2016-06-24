#include <WinSock2.h>
#include <WS2tcpip.h>
#include <string.h>
#include "scheme.h"

int socket_startup()
{
    WSADATA data;
    return WSAStartup(MAKEWORD(2, 2), &data);
}

int make_client_socket(const char* ip, const char* port, int family, int socktype, int flags, int proto)
{
    SOCKET sock = socket(family, socktype, proto);
    int ret, err = 0;
    struct addrinfo hints, *res;

    if (sock == -1)
    {
        err = WSAGetLastError();
        return -err;
    }

    memset(&hints, 0, sizeof(hints));
    hints.ai_family = family;
    hints.ai_socktype = socktype;
    hints.ai_protocol = proto;
    hints.ai_flags = flags;
    
    ret = getaddrinfo(ip, port, &hints, &res);
    if (ret == -1)
    {
        err = ret;
        goto CLOSE_SOCKET;
    }

    Sdeactivate_thread();
    ret = connect(sock, res->ai_addr, sizeof(struct sockaddr));
    Sactivate_thread();

    if (ret != 0)
    {
        err = WSAGetLastError();
        goto FREE_ADDR;
    }

    freeaddrinfo(res);
    return sock;

FREE_ADDR:
    freeaddrinfo(res);
CLOSE_SOCKET:
    closesocket(sock);
    return err > 0 ? -err : err;
}

int make_server_socket(const char* port, int family, int socktype, int proto)
{
    SOCKET sock = socket(family, socktype, proto);
    int b = 1, ret, err = 0;
    struct addrinfo hints, *res;

    if (sock == -1)
    {
        err = WSAGetLastError();
        return -err;
    }

    setsockopt(sock, SOL_SOCKET, SO_REUSEADDR, (char*)(&b), sizeof(b));

    memset(&hints, 0, sizeof(hints));
    hints.ai_family = family;
    hints.ai_socktype = socktype;
    hints.ai_protocol = proto;
    hints.ai_flags = AI_PASSIVE;

    ret = getaddrinfo(NULL, port, &hints, &res);

    if (ret != 0)
    {
        err = ret;
        goto CLOSE_SOCKET;
    }

    ret = bind(sock, res->ai_addr, sizeof(struct sockaddr));

    if (ret != 0)
    {
        err = WSAGetLastError();
        goto FREE_ADDR;
    }

    if (socktype == SOCK_STREAM)
    {
        ret = listen(sock, 24);

        if (ret != 0)
        {
            err = WSAGetLastError();
            goto FREE_ADDR;
        }
    }

    return sock;

FREE_ADDR:
    freeaddrinfo(res);
CLOSE_SOCKET:
    closesocket(sock);
    return err > 0 ? -err : err;
}

int socket_accept(int sock)
{
    struct sockaddr_in addr;
    int len = sizeof(addr);
    Sdeactivate_thread();
    int s = accept(sock, (struct sockaddr*)(&addr), &len);
    Sactivate_thread();
    return s;
}

int socket_send(int sock, ptr bytes, size_t offset, size_t len, int flags)
{
    char unlock = 0;
    if (!Slocked_objectp(bytes))
    {
        Slock_object(bytes);
        unlock = 1;
    }

    Sdeactivate_thread();
    int ret = send(sock, (char*)(Sbytevector_data(bytes)) + offset, len, flags);
    Sactivate_thread();

    if (unlock)
    {
        Sunlock_object(bytes);
    }

    return ret;
}

int socket_recv(int sock, ptr bytes, size_t offset, size_t len, int flags)
{
    char unlock = 0;
    if (!Slocked_objectp(bytes))
    {
        Slock_object(bytes);
        unlock = 1;
    }

    Sdeactivate_thread();
    int ret = recv(sock, (char*)(Sbytevector_data(bytes)) + offset, len, flags);
    Sactivate_thread();

    if (unlock)
    {
        Sunlock_object(bytes);
    }

    return ret;
}

int socket_shutdown(int sock, int how)
{
    return shutdown(sock, how);
}

int socket_close(int sock)
{
    return closesocket(sock);
}
