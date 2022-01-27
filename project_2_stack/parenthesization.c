/**
 * Project 2: Stack ADT
 *
 * Main file for Parasitization calculator
 *
 * Soren Zaiser
 * zais5275@kettering.edu
 *
 * CS231 (Winter 2022)
 * Prof. Vineyard
 */

#include <stdio.h>
#include <string.h>
#include <malloc.h>
#include "stack/stackADT.h"

// Define characters to look for
char *openChars = "([{";
char *closeChars = ")]}";

/**
 * Remove the newline character from a string
 */
void stripNewline(char* toStrip) {
    // Find/replace newline character
    char *nl = strchr(toStrip, '\n');
    if (nl){
        // Replace newline character with null character
        *nl  = '\0';
    }
}

/**
 * Concat validity to end of string
 */
void addValidity(char* str, int lineValid) {
    // Print "Valid" at end of line if valid
    if(lineValid) {
        strcat(str, "✔ Valid\n");
    } else {
        strcat(str, "✖ Invalid\n");
    }
}

/**
 * Add target # of whitespace to the end of a string
 */
void addWhitespace(char* str, unsigned long target) {
    for(int i = 0; i < target ; i++) strcat(str, " ");
}

/**
 * Calculate if character is an opening character
 * @returns Location of character in our character array if found, otherwise -1
 */
int isOpener(char c) {
    // Iterate over all of our opening characters
    for(int i = 0; i < strlen(openChars); i++) {
        // Check if the character equals one of our opening characters
        if (c == openChars[i]) return i;

    }
    // If we get here, character is not an opening character
    return -1;
}

/**
 * Calculate if character is a closing character
 * @returns Location of character in our character array if found, otherwise -1
 */
int isCloser(char c) {
    // Iterate over all of our closing characters
    for(int i = 0; i < strlen(closeChars); i++) {
        // Check if the character equals one of our opening characters
        if (c == closeChars[i]) return i;
    }
    // If we get here, character is not a closing character
    return -1;
}

/**
 * Main function to run parenthesization calculator
 * @param argc Argument Count
 * @param argv Argument Array
 * @return Exit Code, 0 meaning successful
 */
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
    char line[82];

    // Iterate over all rows in the file
    while (fgets(line, 81, inputFilePointer)) {
        // Create a stack
        stackType *stack = create();

        // All line start as valid until we find out they are not
        int lineValid = 1;

        // Iterate over chars in the row
        for(int i = 0; i < strlen(line); i++) {

            // If this character is an opening character, push it to the stack
            if(isOpener(line[i]) > -1) push(stack, line[i]);

            // Check if the current character from the file equals one of our closing characters
            int closerIndex = isCloser(line[i]);
            if(closerIndex > -1) {
                // If this character is a closing character, peek at the top element of the stack
                char temp = peek(stack);

                // Check if the value in the stack matches our opening character
                if(temp == openChars[closerIndex]) {
                    // Valid... pop and continue
                    pop(stack);
                } else {
                    // Invalid, mark and invalid
                    lineValid = 0;

                    // The line is invalid, no need to continue
                    break;
                }
            }
        }

        // Copy line to a larger memory allocation so we can add invalid or valid at end of line
        char *final = (char*)malloc(100);
        strcpy(final, line);

        // Remove newline character
        stripNewline(final);

        // Add whitespace so all valid and invalids are in line
        addWhitespace(final, sizeof line - strlen(final));

        // Add validity to string
        addValidity(final, lineValid);

        // Print final string to file
        fputs(final, outputFilePointer);

        // Destroy stack to keep memory usage low
        destroy(stack);
    }

    // Return 0, happy exit code
    return 0;
}