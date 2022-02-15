/**
 * Project 3: Reverse Words
 *
 * The program checks the length of a word from stdin against the length in the argument
 *
 * Soren Zaiser
 * zais5275@kettering.edu
 *
 * CS231 (Winter 2022)
 * Prof. Vineyard
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

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
        printf("Invalid arguments. Please make sure you call this program with 1 argument: a length.\n");

        // Exit with code 1 to indicate error
        return 1;
    }

    // Convert argument to int
    int target = atoi(argv[1]);

    while(1) {

        // Storage of input
        char input[256] = "";
        // Read Input
        scanf("%s", &input);

        // Check length of line
        if(strlen(input) == target) {
            // If length of input string matches our target, print input to console
            printf("%s\n", input);
        }

    }
}