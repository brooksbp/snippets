/*
 * ptrace-example.c
 *
 * run a process in 'traced' mode in which we're going to single-step
 * through its code - machine code / asm - that gets exec'd by CPU.
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/ptrace.h>
#include <sys/wait.h>
#include <unistd.h>



void run_debugger(pid_t child_pid)
{
    int wait_status;
    unsigned icounter = 0;
    
    printf("debugger started\n");

    /* wait for child to stop on its first instruction */
    wait(&wait_status);

    while (WIFSTOPPED(wait_status)) {
	icounter++;
	
	if (ptrace(PT_STEP, child_pid, 0, 0) < 0) {
	    perror("ptrace: step");
	    return;
	}
	printf("step: %d\n", icounter);
	/* wait for child to stop on its next instruction */
	wait(&wait_status);
	if (icounter == 70) return;
    }

    printf("the child executed %u instructions\n", icounter);
}


void run_target(const char* program_name)
{
    printf("target started. will run '%s'\n", program_name);

    /* activate tracing of this process */
    if (ptrace(PT_TRACE_ME, 0, 0, 0) < 0) {
	perror("ptrace: start tracing");
	return;
    }

    /* replace this processes image with given program */
    execl(program_name, program_name, 0, NULL);
}


/*
 * parent process traces child process that exec's usr-supplied cmd
 */

int main(int argc, char **argv)
{
    pid_t child_pid;

    if (argc < 2) {
	printf("expected a program name as argument!\n");
	return -1;
    }

    child_pid = fork();
    /* fork returns 0 for child  process
                    # for parent process where # is child pid */

    if (child_pid == 0) {
	run_target(argv[1]);
    } else if (child_pid > 0) {
	run_debugger(child_pid);
    } else {
	perror("fork");
	return -1;
    }

    return 0;
}
