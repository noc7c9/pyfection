Pyfection
=

Are you an enlightened programmer that know the evils of braces? Do loathe the fact that you have to write in a c-syntax language that can't compare with the absolute beauty of python?

**Do you pray to the indentation gods to save you from the anathema that is brace-delimited blocks?**

Well your prayers have been answered! Pyfection is a simple little transcompiler that will let you get rid of those ugly braces and write pyfect code instead!

For example, here's a solution for the fizzbuzz problem.
```c
// file: fizzbuzz.c
#include <stdio.h>

/*
 * Fizzbuzz for ints from 1 to 15.
 */
int main(int argv, char *argc[]) {
    for (int i = 1; i <= 15; i++) {
        if (i % 3 != 0 && i % 5 != 0) {
            printf("%d", i);
        }
        if (i % 3 == 0) {
            printf("fizz");
        }
        if (i % 5 == 0) {
            printf("buzz");
        }
        printf(", ");
    }
    printf("\n");

    return 0;
}
```

Look at all those braces! BLEH!
Now have a look a more pyfect version of that code

```c
// file: fizzbuzz.c.pyf
#include <stdio.h>

/*
 * Fizzbuzz for ints from 1 to 15.
 */
int main(int argv, char *argc[])
    for (int i = 1; i <= 15; i++)
        if (i % 3 != 0 && i % 5 != 0)
            printf("%d", i);
        if (i % 3 == 0)
            printf("fizz");
        if (i % 5 == 0)
            printf("buzz");
        printf(", ");
    printf("\n");

    return 0;
```

Absolutely beautiful! Compiling that code is simple too.

```
$ ./pyfect fizzbuzz.c.pyf | gcc -x c - && ./a.out
1, 2, fizz, 4, buzz, fizz, 7, 8, fizz, buzz, 11, fizz, 13, 14, fizzbuzz,
```

Why?
=
This isn't a serious project (duh), it is instead something I chose to make in order to learn [rust](http://rust-lang.org) as well as a chance to parse an indent based language which was definitely interesting.

The inspiration for this came when a friend showed me [Qwerp-Derp/beautifier](https://github.com/Qwerp-Derp/beautifier). So thanks Julian! :)

(It's just a bonus that I get name it such a stupid pun.)
