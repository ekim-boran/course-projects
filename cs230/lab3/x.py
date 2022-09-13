from itertools import permutations
import sys
# Get all permutations of [1, 2, 3]
perm = permutations([1, 2, 3, 4, 5, 6])

print("Border relations with Canada have never been better.")
print("1 2 4 8 16 32")
print("7 327")
print("0 0")
print("ionefg")
print(*list(perm)[int(sys.argv[1])], sep="  ")
print("")
