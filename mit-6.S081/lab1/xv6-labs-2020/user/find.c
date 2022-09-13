#include "../kernel/types.h"
#include "../kernel/stat.h"
#include "../user/user.h"
#include "../kernel/fs.h"
int matchhere(char *, char *);
int matchstar(int, char *, char *);

int match(char *re, char *text)
{
    if (re[0] == '^')
        return matchhere(re + 1, text);
    do
    { // must look at empty string
        if (matchhere(re, text))
            return 1;
    } while (*text++ != '\0');
    return 0;
}

// matchhere: search for re at beginning of text
int matchhere(char *re, char *text)
{
    if (re[0] == '\0')
        return 1;
    if (re[1] == '*')
        return matchstar(re[0], re + 2, text);
    if (re[0] == '$' && re[1] == '\0')
        return *text == '\0';
    if (*text != '\0' && (re[0] == '.' || re[0] == *text))
        return matchhere(re + 1, text + 1);
    return 0;
}

// matchstar: search for c*re at beginning of text
int matchstar(int c, char *re, char *text)
{
    do
    { // a * matches zero or more instances
        if (matchhere(re, text))
            return 1;
    } while (*text != '\0' && (*text++ == c || c == '.'));
    return 0;
}

//int match(char *name, char *target)
//{
//    return strcmp(name, target) == 0;
//}

void find(char *path, char *filename)
{
    char buf[512];
    int fd;

    if ((fd = open(path, 0)) < 0)
    {
        fprintf(2, "find: cannot open %s\n", path);
        return;
    }
    struct stat info;
    if (fstat(fd, &info) < 0)
    {
        fprintf(2, "find: cannot stat %s\n", path);
        close(fd);
        return;
    }

    if (info.type == T_DIR)
    {
        if (strlen(path) + 1 + DIRSIZ + 1 > sizeof buf)
        {
            printf("path too long\n");
        }
        strcpy(buf, path);
        char *p = buf + strlen(buf);
        *p++ = '/';

        struct dirent dir;
        while (read(fd, &dir, sizeof(dir)) == sizeof(dir))
        {
            if (strcmp(dir.name, ".") == 0 || strcmp(dir.name, "..") == 0 || dir.inum == 0)
                continue;
            memmove(p, dir.name, DIRSIZ);
            p[DIRSIZ] = 0;
            if (stat(buf, &info) < 0)
            {
                continue;
            }
            if (info.type == T_FILE && match(dir.name, filename))
            {
                printf("%s\n", (buf));
                continue;
            }

            else if (info.type == T_DIR)
                find(buf, filename);
        }
    }
    close(fd);
}

int main(int argc, char *argv[])
{
    if (argc < 3)
    {
        fprintf(2, "error: arg count\n");
        exit(1);
    }

    find(argv[1], argv[2]);
    exit(0);
}
