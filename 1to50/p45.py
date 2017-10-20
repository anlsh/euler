import math

def is_pentagonal(num):

    root = solve_quadratic(3/2, -1/2, -1*num)[1]
    return (root % 1 == 0) and ((root / 2 * (3 * root - 1)) == num)

def solve_quadratic(a, b, c):

    determinant = math.sqrt(b**2 - (4 * a * c))

    return ((-1*b - determinant) / (2*a),
            (-1*b + determinant) / (2*a))

def solve45():

    n = 144
    while True:
        h = n * (2 * n - 1)
        if is_pentagonal(h):
            return h
        n += 1

if __name__ == "__main__":

    print(solve45())
