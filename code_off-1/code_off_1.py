import sys
from collections import defaultdict


def char_value(c):
    if 'A' <= c <= 'Z':
        return ord(c) - ord('A') + 1
    elif 'a' <= c <= 'z':
        return ord(c) - ord('a')
    else:
        raise ValueError('unexpected character: {!r}'.format(c))


def value(s):
    return sum(char_value(c) for c in s)


def by_value(lines):
    d = defaultdict(set)
    for s in lines:
        d[value(s)].add(s)
    return d


def main():
    n = int(sys.stdin.readline())
    lines = [sys.stdin.readline().rstrip('\n') for _ in range(n)]

    value_map = by_value(lines)
    def equal_strings(s):
        l = list(value_map[value(s)])
        l.remove(s)
        return l

    for s in lines:
        print(s)
        print('true' if s == ''.join(reversed(s)) else 'false')
        for e in sorted(equal_strings(s)):
            print(e)


if __name__ == '__main__':
    main()
