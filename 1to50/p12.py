from functools import reduce

def factor(n):
    """
    Given n, computre its prime factorization and return in list of tuples
    of (prime, exponent) where exponent is nonzero
    TODO Probably needs to be programmed defensively, check that input >1, etc
    """

    prime_factors = []
    divisor = 2
    while n != 1:
        exponent = 0
        while n % divisor == 0:
            exponent += 1
            n = n / divisor

        if exponent > 0:
            prime_factors.append((divisor, exponent))

        divisor += 1

    return prime_factors

def solve12():

    """
    Somewhat efficient way of doing it- a faster way would be to calculate the
    factors of n0 and n1, then diving the intersection of the factors out of
    n2 before factorizing it, but hey it works right now
    """

    n = 1
    while True:
        # Formula for triangle numbers
        tnum = int(n * (n + 1) / 2)
        numdivisors = 1
        for prime, exponent in factor(tnum):
            numdivisors *= (exponent + 1)

        n += 1

        if numdivisors >= 500:
            return tnum

if __name__ == "__main__":

    print(solve12())
