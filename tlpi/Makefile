CC = gcc
CFLAGS = -Wall -O2

LIB_OBJS = error_functions.o get_num.o
LIB_SOCK = inet_sockets.o
LIB_DAEMON = become_daemon.o

vpath %.c = lib/:fileio/:altio/:sockets/:daemons/:sv/:inotify/
INCLUDE = -Ilib/ -Isockets/ -Idaemons/

ALL = copy seek_io t_readv large_file epoll_input id_echo epoll_sv demo_inotify
all: $(ALL)

copy: $(LIB_OBJS) copy.o
	$(CC) $(LIB_OBJS) copy.o -o $@

seek_io: $(LIB_OBJS) seek_io.o
	$(CC) $(LIB_OBJS) seek_io.o -o $@

t_readv: $(LIB_OBJS) t_readv.o
	$(CC) $(LIB_OBJS) t_readv.o -o $@

large_file: $(LIB_OBJS) large_file.o
	$(CC) $(LIB_OBJS) large_file.o -o $@

epoll_input: $(LIB_OBJS) epoll_input.o
	$(CC) $(LIB_OBJS) epoll_input.o -o $@

id_echo: $(LIB_OBJS) $(LIB_SOCK) $(LIB_DAEMON) id_echo_sv.o id_echo_cl.o
	$(CC) $(LIB_OBJS) $(LIB_SOCK) $(LIB_DAEMON) id_echo_sv.o -o id_echo_sv
	$(CC) $(LIB_OBJS) $(LIB_SOCK) $(LIB_DAEMON) id_echo_cl.o -o id_echo_cl

epoll_sv: $(LIB_OBJS) epoll_sv.o
	$(CC) $(LIB_OBJS) epoll_sv.o -o $@

demo_inotify: $(LIB_OBJS) demo_inotify.o
	$(CC) $(LIB_OBJS) demo_inotify.o -o $@

%.o: %.c
	$(CC) $(CFLAGS) $(INCLUDE) -c -o $@ $<

.PHONY: clean
clean:
	@rm -f *.o *~
	@rm -f $(ALL) id_echo_sv id_echo_cl
