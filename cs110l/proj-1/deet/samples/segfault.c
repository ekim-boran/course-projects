#include <stdio.h>

void func2(int a)
{
    printf("About to segfault... a=%d\n", a);
    *(int *)0 = a;
    printf("Did segfault!\n");
}

void func1(int a)
{
    printf("Calling func2\n");
    func2(a % 5);
}
void func3(int a)
{
    printf("xxxxxxxxxxxxxxxx %d\n", a);
}

int main()
{
    for (int i = 0; i < 10; i++)
    {
        func3(i);
    }
    func1(42);
}
