import math

def closest_prime(n):
    diff = n
    primes = sieve(2*n)
    for i in range(len(primes)):
        if diff <= abs(n - primes[i]):
            return primes[i - 1]
        else:
            diff = abs(n - primes[i])


def sieve(n):
    primes = [True] * n
    for p in range(3, int(math.sqrt(n)) + 1, 2):
        if p**2 > n:
            break
        elif primes[p]:
            for i in range(p*p, n, 2*p):
                primes[i] = False
    return [2] + [p for p in range(3, n, 2) if primes[p]]


#---------------------------------------------#

n = int(input())
print(closest_prime(n))

