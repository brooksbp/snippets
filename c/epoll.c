#include <fcntl.h>
#include <sys/epoll.h>



int main (int argc, char **argv)
{
    int epfd;

    epfd = epoll_create(7);

    close(epfd);

    return 0;
}
