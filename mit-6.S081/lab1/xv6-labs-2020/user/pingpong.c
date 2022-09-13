#include "kernel/types.h"
#include "kernel/stat.h"
#include "user/user.h"

int main(int argc, char *argv[])
{

    int c2p[2], p2c[2];
    if (pipe(c2p) == -1)
    {
        fprintf(2, "pipe failed\n");
        exit(1);
    }
    if (pipe(p2c) == -1)
    {
        fprintf(2, "pipe failed\n");
        exit(1);
    }

    int pid = fork();
    if (pid == -1)
    {
        fprintf(2, "fork failed\n");
        exit(1);
    }
    else if (pid == 0)
    {
        // child
        close(c2p[0]);
        close(p2c[1]);

        char message = 0;
        read(p2c[0], &message, 1);
        printf("%d: received ping\n", getpid());
        write(c2p[1], &message, 1);
        exit(0);
    }
    else
    {
        //parent
        close(c2p[1]);
        close(p2c[0]);

        char message = 0;
        write(p2c[1], &message, 1);
        read(c2p[0], &message, 1);
        printf("%d: received pong\n", getpid());
        exit(0);
    }

    exit(0);
}
