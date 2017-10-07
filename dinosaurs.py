# Dailyprogrammer challenge at
#https://www.reddit.com/r/dailyprogrammer/wiki/
#index#wiki_solution_submission_tutorial

import queue

def touch(p1, p2, r):
    """
    return true if circles of radius r centered on p1, p2, intersect
    """
    p1orig = p1
    p2orig = p2
    tmp = None
    if None in p2:
        if None in p1:
            return False
        tmp = p1
        p1 = p2
        p2 = tmp

    R = r

    if p1[0] == None:
        p1 = (p2[0], p1[1])
    elif p1[1] == None:
        p1 = (p1[0], p2[1])
    else:
        R = 2 * r

    toRet = (p1[0] - p2[0])**2 + (p1[1] - p2[1])**2 <= R**2
    return toRet

def neighbors(pointlist, pos, width, length, r):
    """
    Given a list of points, a position, lab dimensions, and radius r,
    find a list of neighbors within 2r distance of the specified point
    """
    nlist = []

    for other in pointlist:
        if touch(pos, other, r) and other != pos:
            nlist.append(other)

    return nlist

def contract(graph, radius):
    """
    Given a graph where neighbors can be > 2r apart, return a
    new graph where they are <= 2r apare
    """
    updated = {}
    for n in graph:
        updated[n] = [x for x in graph[n] if touch(n, x, radius)]

    return updated


def pathexists(startnodes, goalnodes, graph):
    """
    Check if a path exists between nodes 1 and 2 given a graph
    Graph should be in adjacency list form
    """
    connected = set(startnodes)
    q = queue.Queue()
    for c in connected:
        [q.put(n) for n in graph[c]]

    while not q.empty():
        curr = q.get()
        connected.add(curr)
        for n in graph[curr]:
            if n in goalnodes:
                return True
            if n not in connected:
                q.put(n)

    return False

if __name__ == "__main__":

    PRECISION = .0001

    l = input().split(" ")
    numdinos = int(l[0])
    width = int(l[1])
    height = int(l[2])

    dinolist = []

    minx, maxx = width, 0
    miny, maxy = height, 0

    for i in range(numdinos):
        l = input().split(" ")
        coords = tuple((int(c) for c in l))
        minx = min(minx, coords[0])
        maxx = max(maxx, coords[0])
        miny = min(miny, coords[1])
        maxy = min(maxy, coords[1])
        dinolist.append(coords)

    dinolist.append((0, None))
    dinolist.append((width, None))
    dinolist.append((None, 0))
    dinolist.append((None, height))

    minradius = min(minx, width - maxx, miny, height - maxy)
    maxradius = min(height, width)

    maxgraph = {}
    for d in dinolist:
        maxgraph[d] = neighbors(dinolist, d, width, height, maxradius)

    while maxradius - minradius > PRECISION:
        midradius = (minradius + maxradius) / 2
        midgraph = contract(maxgraph, midradius)

        if pathexists([(0, None), (None, height)], [(None, 0), (width, None)],
                      midgraph):

            maxradius = midradius
            maxgraph = midgraph
        else:
            minradius = midradius

    print(minradius)
