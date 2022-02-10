/**
 * Project 3: Parent
 *
 * Main program to run and pipe together caseWorker, reverse, lengthCheck, and wordGrab
 *
 * Soren Zaiser
 * zais5275@kettering.edu
 *
 * CS231 (Winter 2022)
 * Prof. Vineyard
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>

#define READ_END 0
#define WRITE_END 1

int main(int argc, char *argv[]) {
    // Ensure we have 2 arguments
    if (argc < 4) {
        // We're missing arguments, notify user
        printf("Invalid arguments. Please make sure you call this program with all 3 arguments\n");

        // Exit with code 1 to indicate error
        return 1;
    }

    // Get length
    char *targetWordLength = argv[3];

    // Get input/output file strings
    char *inputFile = argv[1];
    char *outputFile = argv[2];

    // Create pipe references
    int wgFd[2];
    int lcFd[2];
    int revFd[2];

    // Create pipes with our file descriptors
    pipe(wgFd); // For wordGrab -> lengthCheck
    pipe(lcFd); // For lengthCheck -> reverse
    pipe(revFd); // For reverse -> caseWorker

    // Fork current thread
    pid_t wgPid = fork();

    // Parent Process (wordGrab)
    if (wgPid > 0) {

        // Close the read end, as we won't be reading anything
        close(wgFd[READ_END]);

        // Duplicate everything going to STDOUT from wordGrab to our pipe
        dup2(wgFd[WRITE_END], STDOUT_FILENO);

        // Construct arguments to run wordGrab
        char *arg[4];
        arg[0] = "./wordGrab";
        arg[1] = inputFile;
        arg[2] = NULL;

        // This child's stdin is now an open pipe to our parent process
        execvp(arg[0], arg);

    // Child Process (Length Check, Reverse, caseWorker)
    } else {

        pid_t lcPid = fork();

        // Parent (lengthCheck)
        if (lcPid > 0) {

            // Pipe the output from wordGrab process to stdin
            dup2(wgFd[READ_END], STDIN_FILENO);

            // Pipe output from this process to our lengthCheck pipe
            dup2(lcFd[WRITE_END], STDOUT_FILENO);

            // Close our pipe to wordGrab, as it's no longer needed after we copied it over to stdout
            close(wgFd[READ_END]);
            close(wgFd[WRITE_END]);

            // Construct arguments to run lengthCheck
            char *arg[4];
            arg[0] = "./lengthCheck";
            arg[1] = targetWordLength;
            arg[2] = NULL;

            // This child's stdin is now an open pipe to our parent process
            execvp(arg[0], arg);

        // Child (reverse, caseWorker)
        } else {

            // Fork process
            pid_t revPid = fork();

            // Parent (reverse)
            if (revPid > 0) {

                // Pipe the output from lengthCheck process to stdin
                dup2(lcFd[READ_END], STDIN_FILENO);

                // Pipe output from this process to our reverse pipe
                dup2(revFd[WRITE_END], STDOUT_FILENO);

                // Close our pipe to lengthCheck, as it's no longer needed after we copied it over to stdout
                close(lcFd[READ_END]);
                close(lcFd[WRITE_END]);

                // Construct arguments to run reverse
                char *arg[4];
                arg[0] = "./reverse";
                arg[1] = NULL;
                arg[2] = NULL;

                // This child's stdin is now an open pipe to our parent process
                execvp(arg[0], arg);

            // Child (caseWorker)
            } else {

                // Pipe the output from reverse process to stdin
                dup2(revFd[READ_END], STDIN_FILENO);

                // Close our pipe to lengthCheck, as it's no longer needed after we copied it over to stdout
                close(revFd[READ_END]);
                close(revFd[WRITE_END]);

                // Construct arguments to run caseWorker
                char *arg[4];
                arg[0] = "./caseWorker";
                arg[1] = outputFile;
                arg[2] = NULL;

                // This child's stdin is now an open pipe to our parent process
                execvp(arg[0], arg);
            }
        }
    }
}