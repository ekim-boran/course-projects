#include "orderBook.h"
#include <stdio.h>
#include <stdlib.h>

typedef struct
{
    int cap;
    int len;
    int *ptr;
} Vec;

Vec *create_vec()
{
    Vec *v = calloc(1, sizeof(Vec));
    v->cap = 4;
    v->len = 0;

    v->ptr = calloc(4, sizeof(int));
    return v;
}
void destroy_vec(Vec *v)
{
    free(v->ptr);
    free(v);
}

int peek(Vec *v, int *n)
{
    if (v->len == 0)
    {
        return 0;
    }
    *n = v->ptr[0];
    return 1;
}

void push(Vec *v, int c)
{
    if (v->len == v->cap)
    {
        v->ptr = realloc(v->ptr, sizeof(int) * v->cap * 2);
        v->cap = v->cap * 2;
    }
    v->ptr[v->len] = c;
    v->len++;

    int index = v->len - 1;
    while (index > 0)
    {
        int parent = (index - 1) / 2;
        if (v->ptr[parent] < v->ptr[index])
        {
            int temp = v->ptr[parent];
            v->ptr[parent] = v->ptr[index];
            v->ptr[index] = temp;
            index = parent;
        }
        else
        {
            break;
        }
    }
}

int pop(Vec *v, int *n)
{
    if (v->len == 0)
    {
        return 0;
    }
    *n = v->ptr[0];
    v->ptr[0] = v->ptr[v->len - 1];
    int index = 0;
    while (1)
    {
        int child1 = ((index + 1) * 2) - 1;
        int child2 = ((index + 1) * 2);
        if (child1 >= v->len)
        {
            break;
        }
        else
        {
            int childindex = 0;
            if ((child2 >= v->len))
            {
                childindex = child1;
            }
            else
            {
                //find the bigger index
                if (v->ptr[child1] > v->ptr[child2])
                {
                    childindex = child1;
                }
                else
                {
                    childindex = child2;
                }
            }

            if (v->ptr[childindex] > v->ptr[index])
            {
                int temp = v->ptr[childindex];
                v->ptr[childindex] = v->ptr[index];
                v->ptr[index] = temp;
                index = childindex;
            }
            else
            {
                break;
            }
        }
    }
    v->len--;

    return 1;
}

struct orderBook
{
    Vec *buys;
    Vec *sells;
};

OrderBook orderBookCreate(void)
{
    OrderBook v = calloc(1, sizeof(struct orderBook));
    v->buys = create_vec();
    v->sells = create_vec();
    return v;
}

void orderBookDestroy(OrderBook order)
{
    destroy_vec(order->buys);
    destroy_vec(order->sells);
    free(order);
}

int orderBookInsert(OrderBook order, int price)
{
    //printf("a\n");

    int item = 0;

    if (price > 0)
    {
        if (peek(order->sells, &item) && price + item > 0)
        {

            int _ = pop(order->sells, &item);
            return item;
        }
        else
        {

            push(order->buys, price);
        }
    }
    if (price <  0)
    {
        if (peek(order->buys, &item) && price + item > 0)
        {
            int _ = pop(order->buys, &item);
            return item;
        }
        else
        {

            push(order->sells, price);
            peek(order->sells, &item);
        }
    }
    return 0;
}

//int main()
//{
//    Vec *v = create_vec();
//    push(v, -1);
//    push(v, -2);
//    push(v, -12);
//    push(v, -123);
//    push(v, -22);
//    push(v, -11);
//    int item = 0;
//    peek(v, &item);
//
//    printf("%d", item);
//
//    while ((pop(v, &item)) != 0)
//    {
//        printf("%d\n", item);
//    }
//}