#define _BSD_SOURCE

#include <assert.h>
#include <netdb.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <signal.h>
#include <fcntl.h>
#include <sys/epoll.h>
#include "tlpi_hdr.h"

#define PORT_NUM  "8080"
#define INT_LEN      30
#define BACKLOG      50
#define MAX_BUF     256
#define MAX_EVENTS    5


#define N_CONN 24

enum conn_state {
    NONE = 0,
    CONNECTED,
};

typedef struct conn_ conn_t;
struct conn_ {
    conn_t *next;
    enum conn_state state;
    int fd;
};

struct conn_ *free_conn;
conn_t conns[N_CONN];

static void conn_free (conn_t *c)
{
    assert(c);
    memset(c, 0, sizeof(conn_t));
    c->next = free_conn;
    free_conn = c;
}

static conn_t *conn_alloc (void)
{
    assert(free_conn);
    conn_t *new_conn = free_conn;
    free_conn = new_conn->next;
    return new_conn;
}

static void conn_init (void)
{
    free_conn = NULL;
    int i;
    for (i = 0; i < N_CONN; i++)
        conn_free(&conns[i]);
}

static void conn_close (conn_t *c)
{
    if (close(c->fd) == -1)
        errExit("close");
    conn_free(c);
}



static void dispatch (conn_t *c)
{
    switch (c->state) {
    case NONE:
    case CONNECTED:
    default:
    }

    int done = 0;
    char buf[MAX_BUF];
    while (1) {
        ssize_t count;
        if ((count = read(c->fd, buf, sizeof(buf))) == -1) {
            if (errno != EAGAIN) {
                errMsg("read");
                done = 1;
            }
            break;
        } else if (count == 0) {
            // EOF. remote has closed connection
            done = 1;
            break;
        }
        
        if (write(1, buf, count) == -1)
            errExit("write");
    }
    if (done)
        conn_close(c);
}



static int make_socket_nonblock (int fd)
{
    int flags;
    if ((flags = fcntl(fd, F_GETFL, 0)) == -1) goto done;
    if (fcntl(fd, F_SETFL, flags |= O_NONBLOCK) == -1) goto done;
    return 0;
 done:
    return -1;
}

int main (int argc, char *argv[])
{
    struct addrinfo hints;
    struct addrinfo *result, *rp;
    int lfd, optval;

    if (signal(SIGPIPE, SIG_IGN) == SIG_ERR)
        errExit("signal");

    memset(&hints, 0, sizeof(struct addrinfo));
    hints.ai_canonname = NULL;
    hints.ai_addr = NULL;
    hints.ai_next = NULL;
    hints.ai_socktype = SOCK_STREAM;
    hints.ai_family = AF_UNSPEC;
    hints.ai_flags = AI_PASSIVE | AI_NUMERICSERV;

    if (getaddrinfo(NULL, PORT_NUM, &hints, &result) != 0)
        errExit("getaddrinfo");

    optval = 1;
    for (rp = result; rp != NULL; rp = rp->ai_next) {
        lfd = socket(rp->ai_family, rp->ai_socktype, rp->ai_protocol);
        if (lfd == -1)
            continue;
        if (setsockopt(lfd, SOL_SOCKET, SO_REUSEADDR, &optval, sizeof(optval)) == -1)
            errExit("setsockopt");
        if (bind(lfd, rp->ai_addr, rp->ai_addrlen) == 0)
            break;
        close(lfd);
    }

    if (rp == NULL)
        fatal("Could not bind socket to any address");

    if (make_socket_nonblock(lfd) == -1)
        errExit("make_socket_nonblock");

    if (listen(lfd, BACKLOG) == -1)
        errExit("listen");

    freeaddrinfo(result);

    int epfd, ready, i;
    struct epoll_event ev;
    struct epoll_event *evlist;

    epfd = epoll_create1(0);
    if (epfd == -1)
        errExit("epoll_create1");

    ev.events = EPOLLIN | EPOLLET;
    ev.data.fd = lfd;
    if (epoll_ctl(epfd, EPOLL_CTL_ADD, lfd, &ev) == -1)
        errExit("epoll_ctl");

    evlist = calloc(MAX_EVENTS, sizeof(struct epoll_event));

    conn_init();

    struct sockaddr_storage claddr;
    socklen_t addrlen;
    int cfd;
#define ADDRSTRLEN (NI_MAXHOST + NI_MAXSERV + 10)
    char addrStr[ADDRSTRLEN];
    char host[NI_MAXHOST];
    char service[NI_MAXSERV];
    
    while (1) {
        ready = epoll_wait(epfd, evlist, MAX_EVENTS, -1);
        if (ready == -1) {
            if (errno == EINTR)
                continue;   /* Restart if interrupted by signal */
            else
                errExit("epoll_wait");
        }
        for (i = 0; i < ready; i++) {
            if (evlist[i].events & EPOLLIN) {
                if (evlist[i].data.fd == lfd) {
                    while (1) {
                        addrlen = sizeof(struct sockaddr_storage);
                        cfd = accept(lfd, (struct sockaddr *) &claddr, &addrlen);
                        if (cfd == -1) {
                            if ((errno == EAGAIN) || (errno == EWOULDBLOCK))
                                break;        /* Processed all incoming conns */
                            else {
                                errMsg("accept");
                                break;
                            }
                        }
                        if (getnameinfo((struct sockaddr *) &claddr, addrlen,
                                        host, NI_MAXHOST, service, NI_MAXSERV, 0) == 0)
                            snprintf(addrStr, ADDRSTRLEN, "(%s, %s)", host, service);
                        else
                            snprintf(addrStr, ADDRSTRLEN, "(?UNKNOWN?)");
                        printf("Connection from %s\n", addrStr);

                        if (make_socket_nonblock(cfd) == -1)
                            errExit("make_socket_nonblock");

                        conn_t *c = conn_alloc();
                        c->state = NONE;
                        c->fd = cfd;

                        ev.data.ptr = (void *) c;
                        ev.events   = EPOLLIN | EPOLLET;
                        if (epoll_ctl(epfd, EPOLL_CTL_ADD, cfd, &ev) == -1)
                            errExit("epoll_ctl");
                    }
                } else {
                    dispatch((conn_t *) evlist[i].data.ptr);
                }
            } else if (evlist[i].events & (EPOLLHUP | EPOLLERR)) {
                printf("ahhh shittt... rework this\n");
                //conn_close(&evli);
            }
        }
    }
    free(evlist);
    close(lfd);
    exit(EXIT_SUCCESS);
}
