input()  # ignore

sticks = list(map(int, input().split(" ")))

while len(sticks) != 0:
    shortest_len = min(sticks)
    cuts = 0
    for i in range(len(sticks)):
        sticks[i] = sticks[i] - shortest_len
        cuts += 1
    print(cuts)
    while 0 in sticks:
        sticks.remove(0)
