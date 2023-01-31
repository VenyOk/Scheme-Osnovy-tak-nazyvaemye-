#!/usr/bin/env python3
import sys
import os

'''
   wc -l <filename> вывести количество строк
   wc -c <filename> вывести количество байт
   wc -m <filename> вывести количество символов
   wc -w <filename> вывести количество слов
'''

# В функциях make_... лучше закрывать файл или открывать через констукцию with ... as
def make_c(path):
    return os.stat(path).st_size


def make_l(path):
    f = open(path)
    return len(f.readlines())


def make_m(path):
    f = open(path)
    return len(f.read())


def make_w(path):
    f = open(path)
    return len(f.read().split())


if __name__ == "__main__":
    sys.stderr = open("Error.log", "w")
    args = sys.argv[1:]
    args.sort()
    list_commands = set()
    file_list = []
    for i in args:
        if i[0] != "-":
            if os.path.exists(i):
                file_list.append(i)
            else:
                print(f"{i}: This file doesn't exist", file=sys.stderr)
        else:
            for j in i[1:]:
                if j not in list_commands:
                    list_commands.add(j)
    if len(list_commands) in [0, 4]:
        itogo = [0] * 4
    else:
        itogo = [0] * len(list_commands)
    for i in file_list:
        ans = []
        if len(list_commands) == 0:
            ans.append(make_c(i))
            ans.append(make_w(i))
            ans.append(make_l(i))
            ans.append(make_m(i))
        else:
            for j in list_commands:
                if j == "c":
                    ans.append(make_c(i))
                elif j == "w":
                    ans.append(make_w(i))
                elif j == "m":
                    ans.append(make_m(i))
                elif j == "l":
                    ans.append(make_l(i))
        ans.sort()
        for z in range(len(ans)):
            itogo[z] += ans[z]
        print(*ans, i)
    if itogo == [0, 0, 0, 0]:
        print("")
    else:
        print("Итого", *itogo)
```
