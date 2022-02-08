cmake_minimum_required(VERSION 3.21)
project(CS231 C)

set(CMAKE_C_STANDARD 11)

# Project 1
# add_executable(CS231 project_1_change_case/main.c)

# Project 2.1 Parenthesization
# add_executable(CS231
#         project_2_stack/parenthesization.c project_2_stack/stack/stack.c project_2_stack/stack/stackADT.h project_2_stack/stack/data_types/doubleType.h project_2_stack/stack/data_types/doubleType.c project_2_stack/stack/data_types/charType.h project_2_stack/stack/data_types/charType.c project_2_stack/postfix_eval.c)

# Project 2.2 Postfix
add_executable(CS231
        project_2_stack/postfix_eval.c project_2_stack/stack/stack.c project_2_stack/stack/stackADT.h project_2_stack/stack/data_types/doubleType.h project_2_stack/stack/data_types/doubleType.c testing_programs/test.c testing_programs/env.c)

# Project 3