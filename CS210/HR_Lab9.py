
def is_palin(l):
    if not l or not l[1:]: return "TRUE"
    elif l[0] == l[-1]:
        return is_palin(l[1:-1])
    else: return "FALSE"

def gcd(a, b):
    if b == 0:
        return a
    else: return gcd(b, a % b)

s = input()
print(is_palin(s))

a = int(input())
b = int(input())
print(gcd(a,b))

