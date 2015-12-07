
def is_palin(l):
    if not l or not l[1:]: return "TRUE"
    elif l[0] == l[-1]:
        return is_palin(l[1:-1])
    else: return "FALSE"

s = input()
print(is_palin(s))

