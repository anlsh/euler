import math

class sparselookup:

    """
    Very naive min-thing-in-range calculator thing made using a "sparse table"
    First off I hash things instead of using a proper array, and also apparently
    there's a better(?) solution using segment trees? Anyways, I should rewrite
    this at some point
    """

    def __init__(self, arr):

        self.arr = arr
        self.minmap = {}

        # Sparse-table-ish stuff, I kinda use a map cuz I'm lazy :|
        for index, element in enumerate(self.arr):
            self.minmap[(index, 1)] = element

        # Update by going from 2^n
        rangelen = 2
        while rangelen < 2 * len(self.arr):

            for index, element in enumerate(self.arr):
                lastlen = int(rangelen / 2)
                self.minmap[(index, rangelen)] = min(self.minmap.get((index, lastlen), math.inf),
                                                     self.minmap.get((index + lastlen, lastlen), math.inf))

            rangelen *= 2

    def minin(self, left, right):
        """
        Return minimum number in the range specified by [left, right)
        """
        right += 1
        diff = right - left
        leftlen = math.pow(2, math.floor(math.log(diff, 2)))
        leftoff = right - (left + leftlen)
        return min(self.minmap[(left, leftlen)], self.minmap[(left + leftoff, leftlen)])

if __name__ == "__main__":
    input()
    numlist = [int(i) for i in input().split(" ")]
    looker = sparselookup(numlist)
    numqueries = int(input())
    for r in range(numqueries):
        lookrange = (int(r) for r in input().split(" "))
        print(looker.minin(*lookrange))
