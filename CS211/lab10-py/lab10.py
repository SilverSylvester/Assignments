import heapq
from itertools import combinations
import sys


def shortest_path(graph, source, target):
    q = [(0, source, [])]
    visited = set()

    while True:
        (weight, v, path) = heapq.heappop(q)
        if v not in visited:
            path = path + [v]
            visited.add(v)
            if v == target:
                return weight, path
            for succ, c in graph[v].items():
                heapq.heappush(q, (weight + c, succ, path))


def main():
    adj_lst = {}
    with open(sys.argv[1]) as f:
        # Gen list of vertices
        vertices = f.readline().strip().split('\t')

        # Gen adjacency list
        for vertex, line in zip(vertices, f):
            formatted = [int(n) for n in line.strip().split('\t')[1:]]
            adj_lst[vertex] = dict(zip(vertices, formatted))

            # Delete nodes that aren't connected
            for k, v in list(adj_lst[vertex].items()):
                if v == 0:
                    del adj_lst[vertex][k]

    distances = [shortest_path(adj_lst, v1, v2)
                 for v1, v2 in combinations(vertices, 2)]

    max_dist = max(distances, key=lambda x: x[0])[0]

    print("Max shortest distance: {}\nCandidate paths:".format(max_dist))
    for d in distances:
        if d[0] == max_dist:
            print(' ——— '.join([str(v) for v in d[1]]))


if __name__ == "__main__":
    main()
