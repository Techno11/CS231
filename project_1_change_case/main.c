/**
 * Project 1: Change Case
 *
 * Soren Zaiser
 * zais5275@kettering.edu
 *
 * CS231 (Winter 2022)
 * Prof. Vineyard
 */

#include <stdio.h>

// Calculate the ASCII distance between the lowercase and uppercase characters  (In case we're working in a wierd character set)
int upperLowerDistance = 'a' - 'A';

// Check if a character is uppercase (between 'A' and 'Z')
int isUpper(char c) {
    return c >= 'A' && c <= 'Z';
}

// Check if a character is lowercase (between 'a' and 'z'
int isLower(char c) {
    return c >= 'a' && c <= 'z';
}

// Process a line, changing case of each alpha character
void processLine(char line[]) {
    // Iterate over known length of array
    for(int i = 0; i < 100; i++) {

        // Check if character is uppercase
        if(isUpper(line[i])) {
            // If character is uppercase, add upper/lower distance
            line[i] += upperLowerDistance;

        // Check if character is lower case
        } else if(isLower(line[i])) {
            // If character is lowercase, subtract upper/lower distance
            line[i] -= upperLowerDistance;
        }

    }
}

int main(int argc, char * argv[]){
    // Ensure we have 3 arguments
    if(argc < 3) {
        // We're missing arguments, notify user
        printf("Invalid arguments. Please make sure you call this program with 2 arguments, an input file as well as an output file.");

        // Exit with code 1 to indicate error
        return 1;
    }
    printf("Found %d arguments. Reading from '%s' and outputting to '%s'. \n", argc - 1, argv[1], argv[2]);

    // Prep pointer for input file stream
    FILE * inputFilePointer, * outputFilePointer;

    // Open Input File in "r" (read) mode
    inputFilePointer = fopen(argv[1], "r");

    // Open Output File in "w" (write) mode
    outputFilePointer = fopen(argv[2], "w");

    // Char array for storing a line of characters
    char line[100];

    // Iterate over all rows in the file
    while (fgets(line, 100, inputFilePointer)) {
        // Process Line
        processLine(line);
        fputs(line, outputFilePointer);
    }

    // Happy Exit Code
    return 0;
}