#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>

static unsigned char *
readAll(FILE *f, size_t *n)
{
    size_t size = 16;
    unsigned char *buffer = malloc(size);
    *n = 0;

    int c;
    while ((c = getc(f)) != EOF)
    {

        if (*n >= size)
        {
            size *= 2;
            buffer = realloc(buffer, size);
        }
        buffer[(*n)++] = c;
    }

    return buffer;
}

typedef struct
{
    int cap;
    int len;
    int *ptr;
} IntVec;

IntVec *create_intvec()
{
    IntVec *v = calloc(1, sizeof(IntVec));
    v->cap = 4;
    v->len = 0;

    v->ptr = calloc(4, sizeof(int));
    return v;
}
void destroy_intvec(IntVec *v)
{
    free(v->ptr);
    free(v);
}

void append_intvec(IntVec *v, int c)
{
    if (v->len == v->cap)
    {
        v->ptr = realloc(v->ptr, sizeof(int) * v->cap * 2);
        v->cap = v->cap * 2;
    }
    v->ptr[v->len] = c;
    v->len++;
}

void append_if_unique(IntVec *v, int item)
{
    for (int k = 0; k < v->len; k++)
    {
        if (v->ptr[k] == item)
        {
            return;
        }
    }
    append_intvec(v, item);
}

typedef struct
{
    int vertices;
    IntVec **edges;
} Graph;

Graph *create_graph(int n)
{
    Graph *v = calloc(1, sizeof(Graph));
    v->vertices = n;

    v->edges = calloc(n, sizeof(IntVec *));
    for (int i = 0; i < n; i++)
    {
        v->edges[i] = create_intvec();
    }
    return v;
}
void destroy_graph(Graph *v)
{
    for (int i = 0; i < v->vertices; i++)
    {
        destroy_intvec(v->edges[i]);
    }
    free(v->edges);
    free(v);
}

void add_edge(Graph *g, int from, int to)
{
    append_intvec(g->edges[from], to);
}

int degree(Graph *g)
{
    int d = 0;
    for (int i = 0; i < g->vertices; i++)
    {
        if (g->edges[i]->len > d)
        {
            d = g->edges[i]->len;
        }
    }
    return d;
}

int is_adj(Graph *g, int a, int b)
{
    for (int i = 0; i < g->edges[a]->len; i++)
    {
        if (b == g->edges[a]->ptr[i])
        {
            return 1;
        }
    }
    return 0;
}

int check_if_okay(Graph *g, int *colors, int vertex_index, int color)
{
    for (int i = 0; i < vertex_index; i++)
    {
        if (color == colors[i] && is_adj(g, i, vertex_index))
        {
            return 0;
        }
    }
    return 1;
}

int color_graph(Graph *g, int *colors, int vertex_index, int number_of_colors)
{
    if (vertex_index == g->vertices)
    {
        return 1;
    }
    for (int color = 0; color < number_of_colors; color++)
    {
        if (check_if_okay(g, colors, vertex_index, color))
        {
            colors[vertex_index] = color;
            int ret = color_graph(g, colors, vertex_index + 1, number_of_colors);
            if (ret)
            {
                return ret;
            }
        }
    }
    return 0;
}

int main()
{
    int vertices;

    if (scanf("%d\n", &vertices) != 1)
    {
        return 0;
    }
    Graph *g = create_graph(vertices);
    int from;
    int to;
    while (scanf("%d %d\n", &from, &to) == 2)
    {
        add_edge(g, from, to);
        add_edge(g, to, from);
    }

    //create another graph from two neigbours
    Graph *two = create_graph(vertices);
    for (int i = 0; i < g->vertices; i++)
    {
        for (int j = 0; j < g->edges[i]->len; j++)
        {
            int neigbour = g->edges[i]->ptr[j];
            for (int k = 0; k < g->edges[neigbour]->len; k++)
            {
                int nneigbour = g->edges[neigbour]->ptr[k];
                if (nneigbour != i)
                {
                    append_if_unique(two->edges[i], nneigbour);
                }
            }
        }
    }
    destroy_graph(g);

    int d = degree(two);
    int maxcolors = 1 + (d * (d - 1));

    int *colors = calloc(vertices, sizeof(int));

    // basic backtracking search
    int ret = color_graph(two, colors, 0, maxcolors);

    for (int i = 0; i < two->vertices; i++)
    {
        printf("%d %d\n", i, colors[i]);
    }

    destroy_graph(two);
    free(colors);
    //for (int i = 0; i < two->vertices; i++)
    //{
    //    for (int j = 0; j < two->edges[i]->len; j++)
    //    {
    //        printf("%d %d\n", i, two->edges[i]->ptr[j]);
    //    }
    //}
}