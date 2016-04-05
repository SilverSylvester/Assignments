import math

N = 400009
p = 1.0
for k in range(1, 216555):
    p *= (N - (k - 1)) / N
    if k % 100 == 0:
        print("{}, {}, {}".format(k, 1 - p, 1 - math.exp(-0.5 * k * (k - 1) / N)))

