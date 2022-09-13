#include "kernel/types.h"
#include "kernel/stat.h"
#include "user/user.h"
void child(int input)
{
    int number;

    int r = read(input, &number, 4);
    if (r <= 0)
    {
        exit(0);
    }
    printf("prime %d\n", number);

    int fds[2];
    if (pipe(fds) == -1)
    {
        fprintf(2, "pipe failed\n");
        exit(1);
    }
    int pid = fork();
    if (pid == 0)
    {
        close(fds[1]);
        child(fds[0]);
    }
    else
    {
        close(fds[0]);

        int k;
        while ((read(input, &k, 4)) == 4)
        {
            if (k % number != 0)
            {
                write(fds[1], &k, 4);
            }
        }

        close(fds[1]);
        int status;
        wait(&status);
    }
}

int main(int argc, char *argv[])
{
    int fds[2];
    if (pipe(fds) == -1)
    {
        fprintf(2, "pipe failed\n");
        exit(1);
    }
    int pid = fork();
    if (pid == 0)
    {
        close(fds[1]);
        child(fds[0]);
    }
    else
    {
        close(fds[0]);

        for (int i = 2; i < 35; i++)
        {
            int k = i;
            write(fds[1], &k, 4);
        }
        close(fds[1]);
        int status;
        wait(&status);
    }
    exit(0);
}
