#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys
import sha3


def f(s):
    k = sha3.keccak_256()
    k.update(s)
    print(k.hexdigest())

if __name__ == '__main__':
    if len(sys.argv) < 2:
        print("{} <input string>".format(sys.argv[0]))
        sys.exit(0)
    f(sys.argv[0].encode("UTF-8"))

