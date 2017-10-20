import math

def solve9():
    trips = gentriples()
    for p in trips:
        a, b, c = p
        if a + b + c == 1000:
            return a * b * c

def gentriples():
    """
    An efficient(?) generator for pythagorean triples
    """
    triples = set()
    a = 2
    while True:
        for b in range(a, math.floor((a**2 - 1) / 2) + 1):
            c = math.sqrt(a**2 + b**2)
            trip = (a, b, int(c))
            if c % 1 == 0 and (trip not in triples):
                triples.add(trip)
                yield trip
        a += 1

if __name__ == "__main__":
    print(solve9())
