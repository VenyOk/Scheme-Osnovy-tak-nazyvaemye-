#!/usr/bin/env python3
import re
import sys

# Генерация словаря для задания с опечатками. Записывает в файл слова с маленькой буквы (через lower). Делал в 3 или 4 ночи, поэтому срамота может здесь обитать
def generate_dictionary(path1, path2):
    mn = []
    file1 = open(path1)
    file2 = open(path2, mode="w")
    for i in file1.readlines():
        lis = re.findall(r'\b(\w+)\b', i)
        for j in lis:
            if j not in mn:
                mn.append(j)
    for i in mn:
        print(i.lower(), file=file2)
    file2.close()
    file1.close()


if __name__ == "__main__":
    args = sys.argv[1:]
    generate_dictionary(args[0], args[1])
