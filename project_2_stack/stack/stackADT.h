/**
 * Project 2: Stack ADT
 *
 * stackADT Header File, contains types for stack.c
 *
 * Soren Zaiser
 * zais5275@kettering.edu
 *
 * CS231 (Winter 2022)
 * Prof. Vineyard
 */

// #include "data_types/charType.h"
#include "data_types/doubleType.h"

typedef struct element {
    struct element *child;
    struct element *parent;
    dataType element;
} elementType;

typedef struct stackHeader {
    struct element *top;
} stackType;

// Push data to top of stack
void push(stackType *, dataType);

// Remove Element from top of stack
dataType pop(stackType *);

// Check if stack is empty
int isEmpty(stackType *);

// Get item on top of stack
dataType peek(stackType *);

// Create an empty stack
stackType *create();

// Free up all memory used by stack
stackType * destroy(stackType *);