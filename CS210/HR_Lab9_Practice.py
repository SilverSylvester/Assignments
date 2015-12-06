
def birthday(n):
    if n < 2: return 1
    else:
        return (366 - n) / 365 * birthday(n - 1)

#------------------------------------#

n = int(input())
print('{0:.3f}'.format(1 - birthday(n)))
