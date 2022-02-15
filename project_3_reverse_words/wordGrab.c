/**
 * Project 3: Reverse Words
 *
 * The program grabs words from a file and prints them on their own line in stdout
 *
 * Soren Zaiser
 * zais5275@kettering.edu
 *
 * CS231 (Winter 2022)
 * Prof. Vineyard
 */

#include <stdio.h>
#include <string.h>

// State Definitions
#define UNKNOWN -1
#define IN_WORD 100
#define IN_SEPARATOR 200


/**
 * Check if a character is a letter
 * @param c Character to check
 * @return 1 if character is letter, 0 if not
 */
int isLetter(char c) {
    return (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z');
}

/**
 * Main function to get words from the file
 * @param argc Argument Count
 * @param argv Argument Array
 * @return Exit Code, 0 meaning successful
 */
int main(int argc, char * argv[]) {
    // Ensure we have 2 arguments
    if (argc < 2) {
        // We're missing arguments, notify user
        printf("Invalid arguments. Please make sure you call this program with 1 argument: an input file.\n");

        // Exit with code 1 to indicate error
        return 1;
    }

    // Prep pointer for input file stream
    FILE *inputFilePointer;

    // Open Input File in "r" (read) mode
    inputFilePointer = fopen(argv[1], "r");

    // Char array for storing a line of characters
    char line[256];

    // Store our current state
    int state = UNKNOWN;

    // Iterate over all rows in the file
    while (fgets(line, 255, inputFilePointer)) {

        // Iterate over all characters in line
        for (int i = 0; i < strlen(line); i++) {

            // Check if character is letter
            if(isLetter(line[i])) {

                // Check our state, if we're in a separator, we need to print a newline
                if(state == IN_SEPARATOR) printf("\n");

                // Print the letter
                fprintf(stdout, "%c", line[i]);

                // Update state
                state = IN_WORD;

            // If character is not letter, we assume to be in a separator
            } else {
                state = IN_SEPARATOR;
            }
        }
    }
    // Empty any buffer to stdout
    fflush(stdout);
}