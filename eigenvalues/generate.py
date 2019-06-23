import numpy, sys

N = int(sys.argv[1])
M = numpy.random.rand(N, N)
for x in numpy.nditer(M, op_flags=['readwrite']):
    x[...] *= numpy.random.rand(1)[0] * 10
M = M + M.T
file = open('in/big.csv', 'w')
numpy.savetxt(file, M, delimiter=',', fmt='%.8f')
