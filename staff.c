#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define CENTER(s1,s2) \
    (((float)(strlen((s1)) + strlen((s2)) + 1)) / 2.0)

float cog (char *s1, int len1,
           char *s2, int len2)
{
    int regular_sum = 0;
    char tmp;
    int z = len1;
    char *p = s1;
    while (z--) {
        tmp = *p++;
        regular_sum += atoi(&tmp);
    }
    z = len2;
    p = s2;
    while (z--) {
        tmp = *p++;
        regular_sum += atoi(&tmp);
    }
    //printf("regular_sum = %d\n", regular_sum);

    int weighted_sum = 0;
    int i;
    p = s1;
    for (i = 1; i <= (len1 + len2); i++) {
        if (i == len1+1)
            p = s2;
        tmp = *p++;
        weighted_sum += i*atoi(&tmp);
    }
    //printf("weighted_sum = %d\n", weighted_sum);

    return ((float)weighted_sum) / ((float)regular_sum);
}

int main (void)
{
    /* int nbytes = 1024; */
    /* char *branch = (char *) malloc(nbytes + 1); */
    /* int nc = 0; */
    /* int i = getchar(); */
    /* while (i != EOF) { */
    /*     nc += 1; */
        
    /* } */

    
    char *s1 = "1119";
    char *s2 = "9111";
    if (CENTER(s1, s2) == cog(s1, strlen(s1), s2, strlen(s2)))
        printf("%d ", (int) strlen(s1));



    return 0;
}
