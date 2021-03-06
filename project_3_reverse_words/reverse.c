/**
 * Project 3: Reverse Words
 *
 * The program reverses all lines inputted through stdin
 *
 * Soren Zaiser
 * zais5275@kettering.edu
 *
 * CS231 (Winter 2022)
 * Prof. Vineyard
 */

#include <stdio.h>
#include <string.h>

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
    // Storage of input
    char input[81] = "";

    // Iterate over stdin while the pipe is open
    while (fgets(input, 80, stdin) != NULL) {

        // Reverse-iterate over the characters, printing them backwards, one-by-one
        for (int i = strlen(input) - 1; i >= 0; i--) {
            fprintf(stdout, "%c", input[i]);
        }

        // Empty any buffer to stdout
        fflush(stdout);
    }

    return 0;
}