/**
 * Project 3: Case Worker
 *
 * Program to read from stdin and output to a file after capitalizing all continents
 *
 * Soren Zaiser
 * zais5275@kettering.edu
 *
 * CS231 (Winter 2022)
 * Prof. Vineyard
 */

#include "stdio.h"
#include "string.h"
#include <ctype.h>


/**
 * Main function to run child processes
 * @param argc Argument Count
 * @param argv Argument Array
 * @return Exit Code, 0 meaning successful
 */
int main(int argc, char * argv[]){
    // Ensure we have 1 argument
    if(argc < 2) {
        // We're missing arguments, notify user
        printf("Invalid arguments. Please make sure you call this program an output file.");

        // Exit with code 1 to indicate error
        return 1;
    }

    // Prep pointer for input file stream
    FILE * outputFilePointer;

    // Open Output File in "w" (write) mode
    outputFilePointer = fopen(argv[1], "w");

    char msg[81];

    while (fgets(msg, 80, stdin) != NULL) {
        // Ignore line ending characters
        if(strcmp(msg, "\n") == 0) continue;

        // Iterate over each character
        for(int i = 0; i < strlen(msg); i++) {
            char c = msg[i];
            if(c == 'a' || c == 'e' || c == 'i' || c == 'o' || c == 'u' || c == 'A' || c == 'E' || c == 'I' || c == 'O' || c == 'U')  {
                msg[i] = tolower(msg[i]);
            } else {
                msg[i] = toupper(msg[i]);
            }
        }

        // Put incoming message to file
        fputs(msg, outputFilePointer);

        // Flush file stream to immediately push to file
        fflush(outputFilePointer);
    }

    // Return 0, happy exit code
    return 0;
}

