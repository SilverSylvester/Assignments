
def parse_in(cmds, stack):
    for cmd in cmds:
        if cmd[0:4] == "PUSH":
            stack.append(int(cmd[5:]))
        elif not stack:
            continue
        else: stack.pop()
    return stack

#----------------------------#

cmds = []
n = int(input())

for i in range(n):
    cmds.append(input())

s = parse_in(cmds, [])

if not s:
    print("empty")
else: print(max(s))

