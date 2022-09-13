#include "kernel/types.h"
#include "kernel/stat.h"
#include "user/user.h"

int main(int argc, char *argv[])
{
    if (argc <= 1)
    {
        fprintf(2, "no args\n");
        exit(1);
    }
    int interval = atoi(argv[1]);
    if (interval == 0)
    {
        exit(0);
    }
    int r = sleep(interval);
    if (r == -1)
    {
        fprintf(2, "sleep failed\n");
        exit(1);
    }
    exit(0);
}
