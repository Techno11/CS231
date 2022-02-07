/**
 * Project 3: Reverse Words
 *
 * Main Program that executes all the child processes
 *
 * Soren Zaiser
 * zais5275@kettering.edu
 *
 * CS231 (Winter 2022)
 * Prof. Vineyard
 */

#include "stdio.h"

/**
 * Main function to run child processes
 * @param argc Argument Count
 * @param argv Argument Array
 * @return Exit Code, 0 meaning successful
 */
int main(int argc, char * argv[]){
    // Ensure we have 3 arguments
    if(argc < 3) {
        // We're missing arguments, notify user
        printf("Invalid arguments. Please make sure you call this program with 3 arguments, an input file, an output file, and a word length.");

        // Exit with code 1 to indicate error
        return 1;
    }
    printf("Found %d arguments. Reading from '%s' and outputting to '%s' with a set word-length of %s \n", argc - 1, argv[1], argv[2], argv[3]);

    // Prep pointer for input file stream
    FILE * inputFilePointer, * outputFilePointer;

    // Open Input File in "r" (read) mode
    inputFilePointer = fopen(argv[1], "r");

    // Open Output File in "w" (write) mode
    outputFilePointer = fopen(argv[2], "w");

    // Return 0, happy exit code
    return 0;
}

