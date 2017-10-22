import heapq

MAX_INDEX = 79
def solvematrix(start, successorfunc, terminal):

    with open("p81to83_matrix.txt") as f:
        grid = f.readlines()
        for index, strline in enumerate(grid):
            grid[index] = [int(s) for s in strline.split(",")]

    unvisited = []
    visited = {}
    heapq.heappush(unvisited, (grid[start[1]][start[0]], start))

    while len(unvisited) > 0:
        cost, node = heapq.heappop(unvisited)
        if node not in visited:
            visited[node] = cost
            if terminal(node):
                return cost
        else:
            continue

        for snodex, snodey in successorfunc(node):
            heapq.heappush(unvisited, (cost + grid[snodey][snodex], (snodex, snodey)))

def successors81(pos):
    """Restriction of successors83"""

    x, y = pos

    snodes = [None]*4
    if x < MAX_INDEX:
        # Right
        snodes[1] = (x + 1, y)
    if y < MAX_INDEX:
        # Down
        snodes[3] = (x, y + 1)

    return [s for s in snodes if s is not None]

def successors82(pos):

    x, y = pos
    snodes = [None]*4

    if x < MAX_INDEX:
        # Right
        snodes[1] = (x + 1, y)
    if y > 0:
        # Up
        snodes[2] = (x, y - 1)
    if y < MAX_INDEX:
        # Down
        snodes[3] = (x, y + 1)

    return [s for s in snodes if s is not None]

def successors83(pos):
    """Given a tuple (x, y), return a list of tuples which
    represent possible successors or some shit"""
    x, y = pos

    snodes = [None]*4
    if x > 0:
        # Left
        snodes[0] = (x - 1, y)
    if x < MAX_INDEX:
        # Right
        snodes[1] = (x + 1, y)
    if y > 0:
        # Up
        snodes[2] = (x, y - 1)
    if y < MAX_INDEX:
        # Down
        snodes[3] = (x, y + 1)

    return [s for s in snodes if s is not None]

def solve81():
    return solvematrix((0, 0), successors81, lambda pos: pos == (MAX_INDEX, MAX_INDEX))

def solve82():
    sweepcosts = []
    for row in range(MAX_INDEX + 1):
        cost = solvematrix((0, row), successors82, lambda pos: pos[0] == MAX_INDEX)
        sweepcosts.append(cost)
    return min(sweepcosts)

def solve83():
    return solvematrix((0, 0), successors83, lambda pos: pos == (MAX_INDEX, MAX_INDEX))

if __name__ == "__main__":

    print(solve81())
