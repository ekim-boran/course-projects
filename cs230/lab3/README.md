# [Lab 3] Bomb lab

## Introduction

The nefarious Dr. Evil has planted a slew of “binary bombs” on our class machines. A binary bomb is a program that consists of a sequence of phases. Each phase expects you to type a particular string on stdin. If you type the correct string, then the phase is defused and the bomb proceeds to the next phase. Otherwise, the bomb explodes by printing "BOOM!!!" and then terminating. The bomb is defused when every phase has been defused.

There are too many bombs for us to deal with, so we are giving each student a bomb to defuse. Your mission, which you have no choice but to accept, is to defuse your bomb before the due date. Good luck, and welcome to the bomb squad!

## Step1: Get Your Bomb

You can obtain your bomb in the provided server. It is in `$HOME/handouts` directory of your provided server. Give the command: `tar -xvf ~/handouts/bomblab.tar` to extract file.

This will create a directory called `./bomblab` with the following files:

* `bomb`: The executable binary bomb
* `bomb.c`: Source file with the bomb’s main routine and a friendly greeting from Dr. Evil.

As soon as you get the files, upload them to your private gitlab repository for storing original files.

## Step2: Defuse Your Bomb

Your job for this lab is to defuse your bomb. You must do the assignment on the provided machine.

You can use many tools to help you defuse your bomb. Please look at the **Advice** section for some tips and ideas. The best way is to use your favorite debugger to step through the disassembled binary.

Although phases get progressively harder to defuse, the expertise you gain as you move from phase to phase should offset this difficulty. However, the last phase will challenge even the best students, so please don’t wait until the last minute to start.

The bomb ignores blank input lines. If you run your bomb with a command line argument, for example,

```bash
linux> ./bomb psol.txt
```

then it will read the input lines from `psol.txt` until it reaches `EOF` (end of file), and then switch over to stdin. In a moment of weakness, Dr. Evil added this feature so you don’t have to keep retyping the
solutions to phases you have already defused.

To avoid accidentally detonating the bomb, you will need to learn how to single-step through the assembly code and how to set breakpoints. You will also need to learn how to inspect both the registers and the memory states. One of the nice side-effects of doing the lab is that you will get very good at using a debugger. This is a crucial skill that will pay big dividends the rest of your career.

## Step3: The Secret Phase

You may get full credit by defusing six phases. However, it is not enough to stop Dr. Evils maleficent plan. In every bomb, there exists a hidden phase which is hard to find: the Secret Phase. Dr. Evil has created this phase as a last resort. For fun, or, for honor of saving our class machine entirely, you can find and defuse the Secret Phase.

## Evaluation

The first four phases are worth 10 points each. Phases 5 and 6 are a little more difficult, so they are worth 15 points each. So the maximum score you can get is **70** points.

Bomb is machine and OS dependent, which means that you should work on the provided server and make sure that your answer works on it. We will test your answer on the same environment that you are provided.

### Autograding

You can grade your answer by typing the following command:

```bash
linux> ./bomb [your_solution_file]
```

Your solution file should follow the following rules:

* Answers for each phase are separated by `\n` (newline chracter)
* It should be ended with `\n` (newline chracter)
* Any other unnecessary character should not exist

For example,

```text
first phase answer
second phase answer
third phase answer
fourth phase answer
fifth phase answer
sixth phase answer
secret phase answer

```

We will use same command to grade your solution.

## Handin Instructions

Go to [the submission website](https://gg.kaist.ac.kr/assignment/28/) and submit the text file that can defuse your bomb.
See [this](https://cp-git.kaist.ac.kr/cs230/cs230#handin-instructions) for more details.

## Advice (Please read this!)

There are many ways of defusing your bomb. You can examine it in great detail without ever running the program, and figure out exactly what it does. This is a useful technique, but it not always easy to do. You can also run it under a debugger, watch what it does step by step, and use this information to defuse it. This is probably the fastest way of defusing it.

There are many tools which are designed to help you figure out both how programs work, and what is wrong when they don’t work. Here is a list of some of the tools you may find useful in analyzing your bomb, and hints on how to use them.

### gdb

The GNU debugger, this is a command line debugger tool available on virtually every platform. You can trace through a program line by line, examine memory and registers, look at both the source code and assembly code (we are not giving you the source code for most of your bomb), set breakpoints, set memory watch points, and write scripts. [The CS:APP web site](http://csapp.cs.cmu.edu/public/students.html) has a very handy single-page gdb summary that you can print out and use as a reference. Here are some other tips for using gdb.

* To keep the bomb from blowing up every time you type in a wrong input, you’ll want to learn how to set breakpoints.
* For online documentation, type “help” at the gdb command prompt, or type “man gdb”, or “info gdb” at a Unix prompt. Some people also like to run gdb under gdb-mode in emacs.

### objdump

#### `objdump -t`

This will print out the bomb’s symbol table. The symbol table includes the names of all functions and global variables in the bomb, the names of all the functions the bomb calls, and their addresses. You
may learn something by looking at the function names!

#### `objdump -d`

Use this to disassemble all of the code in the bomb. You can also just look at individual functions. Reading the assembler code can tell you how the bomb works.

Although objdump -d gives you a lot of information, it doesn’t tell you the whole story. Calls to system-level functions are displayed in a cryptic form. For example, a call to sscanf might appear as:

```
8048c36: e8 99 fc ff ff call 80488d4 <_init+0x1a0>
```

To determine that the call was to sscanf, you would need to disassemble within gdb.

### strings

This utility will display the printable strings in your bomb.

### other references

* [Guide to x86-64](https://web.stanford.edu/class/cs107/guide/x86-64.html)
* [GDB tutorial video 1](https://www.youtube.com/watch?v=svG6OPyKsrw)
* [GDB tutorial video 2](https://www.youtube.com/watch?v=sCtY--xRUyI)
