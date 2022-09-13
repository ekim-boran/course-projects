

#include "pancake.h"
#include <stdio.h>
unsigned int sizes[5] = {3, 3, 2, 3, 1};

void flip(unsigned int *start, unsigned int *end)
{
    while (start < end)
    {
        unsigned int temp = *start;
        *start = *end;
        *end = temp;
        start++;
        end--;
    }
}
void print(unsigned int *start, unsigned int *end)
{
    char c = 0;
    while (start < end)
    {
        if (c == 0)
        {
            printf("%u", *start);
            c = 1;
        }
        else
        {
            printf(" %u", *start);
        }
        start++;
    }
    printf("\n");
}

void runPancakeProgram(unsigned int *memory)
{
    unsigned int pc = 0;
    while (1)
    {
        switch (memory[pc])
        {
        case FLIP:
            flip(memory + memory[pc + 1], memory + memory[pc + 2] - 1);
            pc = pc + 3;
            break;
        case PRINT:
            print(memory + memory[pc + 1], memory + memory[pc + 2]);
            pc = pc + 3;
            break;
        case JUMP:
            pc = memory[pc + 1];
            break;
        case SKIP:
            if (memory[memory[pc + 1]] > memory[memory[pc + 2]])
            {
                pc = pc + 3 + sizes[memory[pc + 3]];
            }
            else
            {
                pc = pc + 3;
            }
            break;
        case STOP:
            return;
        default:
            return;
        }
    }
    printf("pc is %d\n", pc);
}