#include <stdio.h>
#include "csapp.h"
/* Recommended max cache and object sizes */
#define MAX_CACHE_SIZE 1049000
#define MAX_OBJECT_SIZE 102400

// if strict LRU is implemented reader must also acquire writer lock to update usage data
// basic queue - least recently written is implemented
typedef struct entry
{
  char uri[128];
  char *buffer;
  int size;
} CacheEntry;

struct Cache
{
  CacheEntry entries[32];
  pthread_rwlock_t lock;
  int total_size;
  int start;
  int end;
} cache;

void init_cache()
{
  for (int i = 0; i < 32; i++)
  {
    cache.entries[i].size = 0;
    cache.entries[i].buffer = 0;
  }
  pthread_rwlock_init(&cache.lock, NULL);
  cache.total_size = 0;
  cache.start = 0;
  cache.end = 0;
}

int find(char *name)
{
  for (int i = cache.start; i != cache.end; i = (i + 1) % 32)
  {
    if (!strcmp(name, cache.entries[i].uri))
    {
      return i;
    }
  }
  return -1;
}

int read_cache(char *name, char *buf)
{
  int i = 0;
  if ((i = find(name) != -1))
  {
    memcpy(buf, cache.entries[i].buffer, cache.entries[i].size);
    return cache.entries[i].size;
  }
  return 0;
}

void evict()
{
  CacheEntry entry = cache.entries[cache.start];
  free(entry.buffer);
  cache.total_size -= entry.size;
  cache.start = (cache.start + 1) % 32;
}
int find_empty()
{
  int newend = (cache.end + 1) % 32;
  if (newend == cache.start)
  {
    evict();
  }
  return newend;
}

void destroy_cache()
{
  for (int i = cache.start; i != cache.end; i = (i + 1) % 32)
  {
    free(cache.entries[i].buffer);
  }
  cache.start = 0;
  cache.end = 0;
}
void print_cache()
{
  for (int i = cache.start; i != cache.end; i = (i + 1) % 32)
  {
    printf("xx %s %d\n", cache.entries[i].uri, cache.entries[i].size);
  }
}
int write_cache(char *name, char *buf, int size)
{
  if (size > MAX_OBJECT_SIZE)
  {
    return -1;
  }
  while (cache.total_size + size > MAX_CACHE_SIZE)
  {
    evict();
  }
  // if it returns -1 try again
  int index = cache.end;
   cache.total_size += size;
  cache.entries[index].size = size;
  memcpy(cache.entries[index].uri, name, 128);
  cache.entries[index].buffer = (char *)malloc(size);

  memcpy(cache.entries[index].buffer, buf, size);
  cache.end = (cache.end + 1) % 32;

  printf("asdasd\n");
  fflush(stdout);
  return 1;
}
int Read_cache(char *name, char *buf)
{
  int size = 0;
  pthread_rwlock_rdlock(&cache.lock);
  size = read_cache(name, buf);
  pthread_rwlock_unlock(&cache.lock);
  printf("read %s %d\n", name, size);
  return size;
}

void Write_cache(char *name, char *buf, int size)
{
  pthread_rwlock_wrlock(&cache.lock);
  printf("write1 %s %d \n", name, size);
  if (find(name) == -1)
  {
    printf("write2 %s %d \n", name, size);
    write_cache(name, buf, size);
    print_cache();
  }
  pthread_rwlock_unlock(&cache.lock);
}

/* You won't lose style points for including this long line in your code */
static const char *user_agent_hdr =
    "User-Agent: Mozilla/5.0 (X11; Linux x86_64; rv:10.0.3) Gecko/20120305 "
    "Firefox/10.0.3";
int parse_uri(char *uri, char *filename, char *cgiargs)
{
  char *ptr;

  if (!strstr(uri, "cgi-bin"))
  {
    /* Static content */             // line:netp:parseuri:isstatic
    strcpy(cgiargs, "");             // line:netp:parseuri:clearcgi
    strcpy(filename, ".");           // line:netp:parseuri:beginconvert1
    strcat(filename, uri);           // line:netp:parseuri:endconvert1
    if (uri[strlen(uri) - 1] == '/') // line:netp:parseuri:slashcheck
      strcat(filename, "home.html"); // line:netp:parseuri:appenddefault
    return 1;
  }
  else
  { /* Dynamic content */  // line:netp:parseuri:isdynamic
    ptr = index(uri, '?'); // line:netp:parseuri:beginextract
    if (ptr)
    {
      strcpy(cgiargs, ptr + 1);
      *ptr = '\0';
    }
    else
      strcpy(cgiargs, ""); // line:netp:parseuri:endextract
    strcpy(filename, "."); // line:netp:parseuri:beginconvert2
    strcat(filename, uri); // line:netp:parseuri:endconvert2
    return 0;
  }
}

typedef struct _uri
{
  char host[64];
  char path[64];
  char port[8];
} Uri;

typedef struct _request
{
  Uri uri;
  char _uri[128];
  char method[64];
  char version[64];
  char host[64];
  char user_agent[64];
  char accept[64];
  char proxt_connection[64];
  char filename[128];
  char cgiargs[128];
  int is_static;
} Request;

void from_header_line(Request *req, char *buf)
{
  char key[64], value[64];
  sscanf(buf, "%s %s", key, value);
  if (!strcmp(key, "Host:"))
  {
    memcpy(req->host, value, 64);
  }
  else if (!strcmp(key, "User-Agent:"))
  {
    memcpy(req->user_agent, value, 64);
  }
  else if (!strcmp(key, "Accept:"))
  {
    memcpy(req->accept, value, 64);
  }
  else if (!strcmp(key, "Proxy-Connection:"))
  {
    memcpy(req->proxt_connection, value, 64);
  }
}

void parse_request(Request *req, rio_t *rio)
{
  char buf[MAXLINE];
  if (!Rio_readlineb(rio, buf, MAXLINE))
    return;
  sscanf(buf, "%s %s %s", req->method, req->_uri, req->version);
  sscanf(req->_uri, "http://%99[^:]:%99[^/]/%99[^\n]", req->uri.host, req->uri.port, req->uri.path);

  Rio_readlineb(rio, buf, MAXLINE);
  from_header_line(req, buf);

  while (strcmp(buf, "\r\n"))
  {
    Rio_readlineb(rio, buf, MAXLINE);
    from_header_line(req, buf);
  }
  req->is_static = parse_uri(req->_uri, req->filename, req->cgiargs);
}

int create_outgoing_request(Request *req, char *buf)
{
  int n = 0;
  n = sprintf(buf + n, "%s /%s %s\r\n", req->method, req->uri.path, req->version);
  n += sprintf(buf + n, "Host: %s\r\n", req->host);
  n += sprintf(buf + n, "User-Agent: %s\r\n", user_agent_hdr);
  n += sprintf(buf + n, "Connection: Close\r\n");
  n += sprintf(buf + n, "Proxy-Connection: Close\r\n");
  n += sprintf(buf + n, "Accept: %s\r\n", req->accept);
  n += sprintf(buf + n, "\r\n");

  //printf("%s\n", buf);
  return n;
}

void print_request(Request *req)
{
  printf("%s %s %s %s %s\n", req->method, req->uri.host, req->uri.port, req->uri.path, req->version);
  printf("Host %s\n", req->host);
  printf("User-Agent %s\n", req->user_agent);
  printf("Accept %s\n", req->accept);
  printf("Proxy-Connection %s\n", req->proxt_connection);
  printf("Filename %s\n", req->filename);
  printf("Cgiargs %s\n", req->cgiargs);
}

void doit(int fd)
{
  Request req;
  rio_t user_conn;

  char buf[MAX_OBJECT_SIZE];
  Rio_readinitb(&user_conn, fd);
  parse_request(&req, &user_conn);
  int size = 0;
  if ((size = Read_cache(req._uri, buf)) != 0)
  {
    printf("boran %d \n", size);
    rio_writen(fd, buf, size);
    return;
  }

  int serverfd = Open_clientfd(req.uri.host, req.uri.port);

  int n = create_outgoing_request(&req, buf);
  rio_writen(serverfd, buf, n);
  int read = 0;
  rio_t server_conn;
  Rio_readinitb(&server_conn, serverfd);

  char *buf_pointer = buf;
  int total_read = 0;
  while ((read = rio_readlineb(&server_conn, buf_pointer, MAXLINE)) > 0)
  {
    rio_writen(fd, buf_pointer, read);
    buf_pointer = buf_pointer + read;
    total_read += read;
  }
  Write_cache(req._uri, buf, total_read);

  //print_request(&req);
}

void *doit_threaded(void *arg)
{
  int connfd = (int)(long)arg;
  doit(connfd);
  Close(connfd);
  return NULL;
}

int main(int argc, char **argv)
{
  int listenfd, connfd;
  char hostname[MAXLINE], port[MAXLINE];
  socklen_t clientlen;
  struct sockaddr_storage clientaddr;

  /* Check command line args */
  if (argc != 2)
  {
    fprintf(stderr, "usage: %s <port>\n", argv[0]);
    exit(1);
  }

  listenfd = Open_listenfd(argv[1]);
  pthread_t pid;
  while (1)
  {
    clientlen = sizeof(clientaddr);
    connfd = Accept(listenfd, (SA *)&clientaddr,
                    &clientlen); // line:netp:tiny:accept
    Getnameinfo((SA *)&clientaddr, clientlen, hostname, MAXLINE, port, MAXLINE,
                0);
    printf("proxy: Accepted connection from (%s, %s)\n", hostname, port);

    Pthread_create(&pid, 0, doit_threaded, (void *)(long)connfd);
    Pthread_detach(pid);
  }
  destroy_cache();
  return 0;
}
