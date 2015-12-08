from collections import deque

def parse_in(cmds, queue):
    for cmd in cmds:
        if cmd[0:6] == "INSERT":
            queue.append(cmd[7:])
        elif not queue:
            continue
        else: queue.popleft()
    return queue

def middle(xs):
    if not xs: return "empty"
    elif len(xs) % 2:
        return xs[len(xs) // 2]
    else: return xs[len(xs) // 2 - 1]
        

#--------------------------------#

n = int(input())
cmds = []

for i in range(n):
    cmds.append(input())

print(middle(parse_in(cmds, deque([]))))

