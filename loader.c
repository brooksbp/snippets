#include <stdio.h>
#include <libelf.h>

int main (void)
{
    
    if (elf_version(EV_CURRENT) == EV_NONE) {
	printf("libelf failed to load\n");
    }

    return 0;
}

/*
# Local Variables:
# compile-command: "gcc loader.c -o loader -O3 -I/opt/local/include/ -I/opt/local/include/libelf/"
# End:
*/
