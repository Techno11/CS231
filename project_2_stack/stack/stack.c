/**
 * Project 2: Stack ADT
 *
 * stack.c, contains data to create and modify a stack
 *
 * Soren Zaiser
 * zais5275@kettering.edu
 *
 * CS231 (Winter 2022)
 * Prof. Vineyard
 */

#include <malloc.h>
#include <stdlib.h>
#include "stackADT.h"


// Push data to top of stack
void push(stackType * myStack, dataType myData) {
    // Check that the list exists and is insertable
    if (myStack == NULL) {
        fprintf(stderr, "List is null, can not insert item into.\n");
        exit(1);
    }

    // Setup pointer to old top
    elementType * oldTop = myStack->top;

    // Allocate memory for our new top
    elementType * newTop = (elementType *) malloc(sizeof(elementType));

    // Set value of new element
    newTop->element = myData;
    newTop->child = NULL;
    newTop->parent = NULL;

    // If the list is not empty, setup parent/child pointers
    if(!isEmpty(myStack)) {
        oldTop->parent = newTop;
        newTop->child = oldTop;
    }

    // Set new top on stack header
    myStack->top = newTop;
}

// Remove Element from top of stack
dataType pop(stackType * myStack) {
    // Return null if stack is empty
    if(myStack == NULL) return -1;
    if(isEmpty(myStack)) return -1;

    // Setup pointers
    elementType *currentElement = myStack->top;
    elementType *nextElement = currentElement->child;

    // Temp store data
    dataType data = currentElement->element;

    // Free memory used by the top element
    free((void *) currentElement);

    // Check if next element is null
    if(nextElement != NULL) {
        // Update Parent reference on new top element
        nextElement->parent = NULL;

        // Set the top to the new top
        myStack->top = nextElement;
    } else { // If next element is null, then we're at the end of the stack! Mark the top as null
        myStack->top = NULL;
    }

    return data;
}

// Check if stack is empty
int isEmpty(stackType * myStack) {
    // If our top element is null, the stack is empty
    return myStack->top == NULL;
}

// Get item on top of stack
dataType peek(stackType * myStack) {
    // Get top element
    elementType * top = myStack->top;

    if(top == NULL) return -1;

    // Return it's value
    return top->element;
}

// Create an empty stack
stackType * create() {
    // Allocate Memory for our new stack
    stackType * myStack;
    myStack = (stackType *) malloc(sizeof(stackType));

    // Check that our stack is not null, and that the memory allocation worked
    if(myStack != NULL) {
        myStack -> top = NULL;
    }
    return myStack;
}

// Free up all memory used by stack
stackType * destroy(stackType *myStack){
    elementType * currentElement;
    elementType * nextElement;

    // Check that the stack isn't empty
    if (!isEmpty(myStack)) {
        // Setup references for when we start cleaning
        currentElement = myStack -> top;
        nextElement = currentElement -> child;

        // Loop over all elements in stack until we run out
        while(nextElement != NULL) {
            // Free memory
            free((void *) currentElement);
            // Update references
            currentElement = nextElement;
            nextElement = currentElement -> child;
        }

        // Clear the last element
        free ((void *) currentElement);
    }

    // Lastly, clear the space taken up by our stack header
    free ((void *) myStack);
    myStack = NULL;
    return myStack;
}