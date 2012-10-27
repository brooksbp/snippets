#include <signal.h>
#include <syslog.h>
#include <sys/wait.h>
#include "become_daemon.h"
#include "inet_sockets.h"
#include "tlpi_hdr.h"

#define SERVICE "echo"
#define BUF_SIZE 4096

static void            /* SIGCHLD handler to reap dead child processes */
grimReaper(int sig)
{
    int savedErrno;            /* Save 'errno' in case changed here */

    savedErrno = errno;
    while (waitpid(-1, NULL, WNOHANG) > 0)
        continue;
    errno = savedErrno;
}

/* Handle a client request: copy socket input back to socket */

static void
handleRequest(int cfd)
{
    char buf[BUF_SIZE];
}
