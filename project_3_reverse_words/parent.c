/*
Interprocess Communication Using Pipes
Remove vowels from words in child process.
Parent uses pipe to send a string to child.
Child processes string and writes result to screen.
*/
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>

void nullString(char * c, int size)
{
    int i;
    for(i = 0; i < size; i++)
        *c = '\0';
}

int main() {
    pid_t pid;
    int fd[2];
    char msg[100];
    int len;
    nullString(msg, 100);
    pipe(fd);
    pid = fork();
    // Parent Process
    if (pid > 0) {
        // Close read end, child reads this
        close(fd[0]);


        while (fgets(msg, 100, stdin) != NULL) {
            len = strlen(msg);
            write(fd[1], msg, len);
        }
        close(fd[1]);//close remaining pipe end
        return 0;
    // Child Process
    } else {
        // Close write end, parent writes this
        close(fd[1]);
        dup2(fd[0], STDIN_FILENO);

        // Not needed after dup2
        close(fd[0]);

        // This child's stdin is now an open pipe to our parent process
        execle("./reverse","env", NULL, NULL);
        return 0;
    }
}