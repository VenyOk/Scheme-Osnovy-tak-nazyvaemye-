#!/usr/bin/env python3
import sys
import string

# Основная часть поиска опечаток. generate_dict.py генерирует словарь, а speller.py ищет опечатки
if __name__ == "__main__":
    args = sys.argv
    file1 = open(args[1])
    file2 = open(args[2])
    lis1 = file1.readlines()
    lis2 = file2.readlines()
    for i in range(len(lis2)):
        s = ''
        k = 0
        for j in lis2[i]:
            searched = False
            if j in string.ascii_letters:
                s += j
            else:
                if s != '':
                    for z in lis1:
                        if s.lower() == z[:len(z) - 1]:
                            searched = True
                            break
                    if not searched:
                        print(f'{i + 1}, {k + 1 - len(s)} {s}')
                s = ''
            k += 1
    file1.close()
    file2.close()
