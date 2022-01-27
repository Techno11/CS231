/**
 * Project 2: Stack ADT
 *
 * Main file for Postfix Evaluation calculator
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
#include "stack/stackADT.h"

// All characters of note
char operandMin = '0';
char operandMax = '9';
char *delimiterCharacters = " ";

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
 * Add target # of whitespace to the end of a string
 */
void addWhitespace(char* str, unsigned long target) {
    for(int i = 0; i < target ; i++) strcat(str, " ");
}

/**
 * Main function to run postfix calculator
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

        // Strip newline character for applicable lines
        stripNewline(line);

        // Store line for later, in a larger memory allocation to allow for result
        char tempLine[120];
        strcpy(tempLine, line);

        // Split string on delimiter character
        char * token = strtok(line, delimiterCharacters);

        // Until there are no more tokens in the line
        while (token != NULL){

            // Check if is operand
            if (token[0] >= operandMin && token[0] <= operandMax) {
                double temp = atof(token);
                push(stack, temp);
            } else if(strlen(token) > 0) {
                // Get last 2 numbers from stack
                double y = pop(stack);
                double x = pop(stack);

                // Check that y and x exist and are valid
                if(y > -1 && x > -1) {
                    // Variable to store result
                    double final = -1;
                    switch(token[0]) {
                        // Process addition
                        case '+': {
                            final = x + y;
                            break;
                        }
                        // Process Subtraction
                        case '-': {
                            final = x - y;
                            break;
                        }
                        // Process Multiplication
                        case '*': {
                            final = x * y;
                            break;
                        }
                        // Process Division
                        case '/': {
                            final = x / y;
                            break;
                        }
                        // Unrecognised Character
                        default: {
                            printf("Unknown string: '%s' \n", token);
                            break;
                        }
                    }
                    // Push final value to the stack
                    push(stack, final);
                } else { // Invalid postfix
                    printf("Invalid postfix near '%s' \n", token);
                }
            }

            // Process next token in string
            token = strtok(NULL, " ");
        }

        // Get final answer
        double result = pop(stack);

        // Add whitespace so all valid and invalids are in line
        addWhitespace(tempLine, sizeof line - strlen(tempLine));

        // Add output
        sprintf(tempLine,"%s = %f\n", tempLine, result);

        // Print final string to file
        fputs(tempLine, outputFilePointer);

        // Destroy stack
        destroy(stack);
    }

    // Return 0, happy exit code
    return 0;
}