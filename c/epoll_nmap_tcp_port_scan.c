#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/resource.h>
#include <sys/types.h>
#include <sys/epoll.h>
#include <sys/socket.h>
#include <netdb.h>
#include <time.h>

static void errExit(const char *fmt, ...);

struct timespec diff_ts(struct timespec *a, struct timespec *b)
{
  struct timespec c;
  if ((b->tv_nsec - a->tv_nsec) < 0) {
    c.tv_sec  = b->tv_sec  - a->tv_sec  - 1;
    c.tv_nsec = b->tv_nsec - a->tv_nsec + 1000000000;
  } else {
    c.tv_sec  = b->tv_sec  - a->tv_sec;
    c.tv_nsec = b->tv_nsec - a->tv_nsec;
  }
  return c;
}

static inline int ts_to_ms(struct timespec *ts)
{
  return (ts->tv_sec * 1000) + (ts->tv_nsec / 1000000);
}

int getsockopt_error(int fd)
{
  int optval;
  socklen_t optlen;

  optlen = sizeof(optval);

  if (getsockopt(fd, SOL_SOCKET, SO_ERROR, &optval, &optlen) == -1)
    errExit("getsockopt\n");

  return optval;
}

typedef struct {
  int epfd;
  int count;
} async_ctx_t;

typedef enum {
  CONN_NONE,
  CONN_CONNECTING,
  CONN_ESTABLISHED,
  CONN_FAILED
} conn_state_e;

typedef struct {
  conn_state_e state;
  struct sockaddr_in saddr;
  int fd;
} conn_t;

void conn_new(conn_t *c, uint16_t port, struct in_addr *ia)
{
  c->state = CONN_NONE;

  c->fd = socket(AF_INET, SOCK_STREAM | O_NONBLOCK, 0);
  if (c->fd == -1)
    errExit("socket errno=%d(%s)\n", errno, strerror(errno));

  memset(&c->saddr, 0, sizeof(struct sockaddr_in));
  c->saddr.sin_family = AF_INET;
  c->saddr.sin_port   = htons(port);
  c->saddr.sin_addr   = *ia;
}

void conn_free(conn_t *c)
{
  if (c->fd)
    close(c->fd);
}

int conn_connect(conn_t *c, async_ctx_t *async)
{
  assert(c->fd);
  assert(c->state == CONN_NONE);

  if (connect(c->fd, (struct sockaddr *) &c->saddr,
              sizeof(struct sockaddr_in)) != -1) {
    /* Connection established immediately. */
    c->state = CONN_ESTABLISHED;
    /* Close fd here since only interested in whether connect() succeeded. */
    conn_free(c);
    return 0;
  } else if (errno == EINPROGRESS) {
    /* File descriptor for socket will be ready for writing when connection
     * established asynchronously. */
    struct epoll_event ev;

    ev.events   = EPOLLIN | EPOLLET;
    ev.data.fd  = c->fd;
    ev.data.ptr = c;

    assert(async->epfd != c->fd);

    if (epoll_ctl(async->epfd, EPOLL_CTL_ADD, c->fd, &ev) == -1)
      errExit("epoll_ctl errno=%d(%s)\n", errno, strerror(errno));
    async->count++;

    c->state = CONN_CONNECTING;
    return 0;
  }
  return -1;
}

void handle_conn(conn_t *c, async_ctx_t *async, uint32_t events)
{
  assert(c->state == CONN_CONNECTING);
  assert(c->fd);

  async->count--;

  if (events & (EPOLLRDHUP | EPOLLHUP | EPOLLERR)) {
    /* Asynchronous connect() failed. */
    c->state = CONN_FAILED;
    conn_free(c);
  } else {
    c->state = CONN_ESTABLISHED;
    /* Just close the fd here since we're only interested in whether we could
     * connect or not. Ideally we would EPOLL_CTL_DEL and leave the connection
     * open for future use. */
    conn_free(c);
  }
}

/* Get current process' max number of fds.  Increase if possible. */
static int get_max_num_fds(void)
{
  struct rlimit r;

  if (getrlimit(RLIMIT_NOFILE, &r) == -1)
    errExit("getrlimit errno=%d(%s)\n", errno, strerror(errno));

  if (r.rlim_cur < r.rlim_max && r.rlim_max != RLIM_INFINITY) {
    r.rlim_cur = r.rlim_max;

    if (setrlimit(RLIMIT_NOFILE, &r) == -1)
      errExit("setrlimit errno=%d(%s)\n", errno, strerror(errno));
  }

  if (getrlimit(RLIMIT_NOFILE, &r) == -1)
    errExit("getrlimit errno=%d(%s)\n", errno, strerror(errno));

  return r.rlim_cur - 1;
}


/* Number of connections to make. This is really just the upper bound of the
 * port scan: 0 .. n */
#define NUM_CONNS 65535

/* Max num events returned by epoll_wait(). */
#define NUM_EVENTS 2048

/* Max num consecutive epoll timeouts before declaring rest of connections valid. */
#define MAX_EPOLL_TIMEOUTS 12

#define EPOLL_TIMEOUT 125


int main(int argc, char *argv[])
{
  async_ctx_t async;
  conn_t conns[NUM_CONNS];
  int i, k;

  if (argc < 2)
    errExit("Usage: %s <ipv4-address>\n", argv[0]);

  memset(conns, 0, NUM_CONNS * sizeof(conn_t));

  async.count = 0;
  async.epfd = epoll_create(NUM_EVENTS);
  if (async.epfd == -1)
    errExit("epoll_create");

  struct in_addr ia;
  if (inet_pton(AF_INET, argv[1], &ia) <= 0)
    errExit("inet_pton errno=%d(%s)\n", errno, strerror(errno));

  /* Max number of fds for connections (minus std fds). */
  int max_conns = get_max_num_fds() - 3;

  int c_iter = 0;
  int send_count, recv_count;
  int epoll_hyst = 0;

  while ((c_iter < NUM_CONNS) || (async.count > 0)) {
    send_count = 0;
    recv_count = 0;

    /* Kick off connections. */
    if (c_iter < NUM_CONNS) {
      while ((c_iter + send_count < NUM_CONNS) &&  /* there is work */
             (async.count < max_conns))            /* have not exhausted avail fds */
      {
        conn_t *c = &conns[c_iter + send_count];
        conn_new(c, c_iter + send_count /*port*/, &ia);
        if (conn_connect(c, &async) == -1)
          errExit("conn_connect errno=%d(%s)\n", errno, strerror(errno));
        send_count++;
      }
      c_iter += send_count;
    }

    /* Need to re-think ideal num of concurrent connections, RTT latency,
     * send/recv rate, burstiness, etc. instead of max concurrent connections
     * and blindly sleeping here. */
    usleep(500000);

    struct epoll_event ev[NUM_EVENTS];

 again:
    recv_count = epoll_wait(async.epfd, ev, NUM_EVENTS, EPOLL_TIMEOUT);
    if (recv_count == -1) {
      if (errno == EINTR)
        goto again;
      else
        errExit("epoll_wait errno=%d(%s)\n", errno, strerror(errno));
    }

    if (recv_count > 0) {
      for (k = 0; k < recv_count; k++)
        handle_conn((conn_t *) ev[k].data.ptr, &async, ev[k].events);

      //printf("handled %d conns... async.count=%d\n", k, async.count);
      epoll_hyst = 0;
    } else {
      epoll_hyst++;
    }

    if (epoll_hyst < MAX_EPOLL_TIMEOUTS)
      continue;

    /* Max epoll timeouts reached, mark rest of connections as established.. */
    for (i = 0; i < c_iter; i++) {
      conn_t *c = &conns[i];

      if (c->state != CONN_CONNECTING)
        continue;

      c->state = CONN_ESTABLISHED;
      conn_free(c);
      async.count--;
    }
  }

  /* Print established connections. */
  for (i = 0; i < NUM_CONNS; i++) {
    conn_t *c = &conns[i];
    if (c->state == CONN_ESTABLISHED)
      printf("%d/tcp\n", ntohs(c->saddr.sin_port));
  }

  exit(EXIT_SUCCESS);
}

static void errExit(const char *fmt, ...)
{
  va_list alist;

  fflush(stdout);

  va_start(alist, fmt);
  vfprintf(stderr, fmt, alist);
  va_end(alist);

  fflush(stderr);
  exit(EXIT_FAILURE);
}
