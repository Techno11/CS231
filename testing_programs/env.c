#include <stdio.h>
int main(int argc, char * argv[], char * envp[])
{
    printf("child process says: %s\n", envp[0]);

    char msg[100];
    while (fgets(msg, 100, stdin) != NULL)
    {
        printf("child read: %s\n",msg);
    }
}