# [Lab 1] Linked List Lab

## Introduction

The purpose of this assignment is to become more familiar with using C programming language that you will be using throughout this course.

Your task is to fill in the skeleton code and implement the doubly linked list in C.

## Handout Instructions

Fork this repository to your private namespace; make it private; and then clone it. See [this](https://cp-git.kaist.ac.kr/cs230/cs230#tools) for more details.

## Files you need to modify

The only file you will be modifying is `list.c`.

You need to fill in all the functions containing `TODO: replace it with your code` comment in `list.c`.

Do not change any interface and datatype in `list.c`.

You may modify the `test.c` and add the `iterate_print_keys()` function to help you with debugging. However, you must **make sure your tests work with the original `test.c` file.**

## Doubly Linked List

For this lab, you need to implement [doubly linked list](https://en.wikipedia.org/wiki/Doubly_linked_list) with following chracteristics:

* Each node points to its predecessor node with `prev`, and successor node with `next`
* Two sentinel nodes, `head` and `tail` node which represents the beginning and the end of the list respectively.
* Sentinel nodes do not hold any meaningful key values, just `-1` as a key.
* `tail`’s next node is `head` and head’s previous node is tail, which makes it easier for head and tail to find each other.

The functions that needed to be implemented are provided in the `list.c` file. The requirements of each skeleton function is described in the block comment above the skeleton function. You may see the comments in the `list.h` file.

## Evaluation

Your score will be computed out of a maximum of **80** points:

* **80** Correctness points: 8 tests at 10 points each.
* **Style**: submitted code must be formatted according to the llvm style. Otherwise, you will get 0 points. **Don't forget to format your code before submission using `make format`.**

### Autograding

You can grade your file by executing `make grade`.

It evaluates your code style and program correctness by executing `make format-check` and `make test_run` respectively.

Your solution will be tested on a same Linux machine that you were provided, using the same testing script that were included in your lab directory.

## Handin Instructions

Go to [the submission website](https://gg.kaist.ac.kr/assignment/26/) and submit your `list.c`.
See [this](https://cp-git.kaist.ac.kr/cs230/cs230#handin-instructions) for more details.

## Advice

* Use [`malloc`](https://en.cppreference.com/w/c/memory/malloc) and [`free`](https://en.cppreference.com/w/c/memory/free) to allocate and free memory.

* You can set your iteration to begin at the head and end at the tail.
