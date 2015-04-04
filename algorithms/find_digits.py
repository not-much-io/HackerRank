tcs = int(input())
ints = (input() for i in range(tcs))

def to_digits(n):
    return (int(c) for c in n)

def is_divisor(d, n):
    if d == 0:
        return False
    return True if (n % d) == 0 else False
    
def exact_divisor_digits(n):
    digits = to_digits(n)
    return len(list(filter(lambda d: is_divisor(d, int(n)), digits)))
        
for i in ints:
    print(exact_divisor_digits(i))
