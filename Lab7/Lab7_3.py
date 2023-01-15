#!/usr/bin/env python3
import generator
import sys
if __name__ == "__main__":
    res = generator.gen(int(sys.argv[1]), int(sys.argv[2]))
    for i in range(len(res)):
        print(res[i])
