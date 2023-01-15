import random
import string


def gen(length, count):
    symbols = string.printable
    res = []
    for i in range(count):
        s = ""
        for j in range(length):
            s += random.choice(symbols)
        res.append(s)
    return res
