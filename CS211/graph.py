# pylint: disable-all

from collections import deque
import sys

def find_path_dfs(graph, start, end, current=None, path=[]):
    if not current:
        current = start
    path += [current]
    if current != end and not graph[current]:
        return None
    if current == end:
        return path
    for node in graph[current]:
        if not find_path_dfs(graph, start, end, node, path):
            path.pop()
        else:
            return path
    path = [start]

def find_path_bfs(graph, start, end):
    q = deque([(start, [start])])
    while q:
        node, path = q.popleft()
        for n in graph[node] - set(path):
            if n == end:
                return path + [n]
            else:
                q.append((n, path + [n]))

if __name__ == "__main__":
    graph = { 'A': set(['K', 'G', 'I'])
            , 'B': set([])
            , 'C': set([])
            , 'D': set(['M'])
            , 'E': set([])
            , 'F': set([])
            , 'G': set(['B', 'C'])
            , 'H': set([])
            , 'I': set(['H', 'D'])
            , 'J': set(['L'])
            , 'K': set(['F', 'E', 'J'])
            , 'L': set([])
            , 'M': set([]) }

    start = input("Find path from ")
    end = input("to ")
    print("Depth first search: {}".format(find_path_dfs(graph, start, end)))
    print("Breadth first search: {}".format(find_path_bfs(graph, start, end)))

