#!/usr/bin/env python3
import time
import sys


def memoize(function):
    values = {}

    def init(*args):
        if args in values:
            return values[args]
        else:
            values[args] = function(*args)
            return values[args]
    return init


def fib(n):
    if n == 0:
        return 0
    elif n == 1:
        return 1
    else:
        return fib(n-1) + fib(n-2)


@memoize
def fast_fib(n):
    if n == 0:
        return 0
    elif n == 1:
        return 1
    else:
        return fast_fib(n-1) + fast_fib(n-2)
# Время по приколу считается, чтобы показать разницу

if __name__ == "__main__":
    start = time.time()
    res2 = fast_fib(int(sys.argv[1]))
    end = time.time() - start
    print("С мемоизацией:", res2, "-", end)

    start = time.time()
    res1 = fib(int(sys.argv[1]))
    end = time.time() - start
    print("Без мемоизации:", res1, "-", end)
