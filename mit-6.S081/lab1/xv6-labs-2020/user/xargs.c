#include "../kernel/types.h"
#include "../kernel/param.h"

#include "../kernel/stat.h"
#include "../user/user.h"

char *
getword(char *buf)
{
    int i = 0;
    char c;
    while (read(0, &c, 1) >= 1)
    {
        if (c == '\n' || c == ' ' || c == '\r')
            break;
        buf[i++] = c;
    }
    buf[i] = '\0';
    return buf;
}

int main(int argc, char *argv[])
{
    char *argvs[MAXARG];
    for (int i = 1; i < argc; i++)
    {
        argvs[i - 1] = argv[i];
    }
    int index = argc - 1;
    char buf[512];
    char *p = buf;
    while (1)
    {
        p = getword(p);
        if (p[0] == '\0')       
            break;
        

        argvs[index] = p;
        index++;
        p = p + strlen(p) + 1;
    }
    argvs[index] = (char *)0;
    exec(argv[1], argvs);
    printf("exec failed\n");
    exit(0);
}
