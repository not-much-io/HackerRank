import math

tcs = int(input())
fibos = (int(input()) for i in range(tcs))

def is_fibonacci(n):
    phi = 0.5 + 0.5 * math.sqrt(5.0)
    a = phi * n
    if n == 0 or abs(round(a) - a) < 1.0 / n:
        return "IsFibo"
    else:
        return "IsNotFibo"
    
for f in fibos:
    print(is_fibonacci(f))
