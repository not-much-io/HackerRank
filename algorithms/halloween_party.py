import math

nr_tcs = int(input())
tcs = (int(input()) for i in range(nr_tcs))


def pieces_from_cuts(cuts):
    return math.floor(cuts * cuts / 4)

for tc in tcs:
    print(int(pieces_from_cuts(tc)))
