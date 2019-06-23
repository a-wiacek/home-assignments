import numpy, sys

N = int(sys.argv[1])
M = numpy.random.rand(N, N)
for x in numpy.nditer(M, op_flags=['readwrite']):
    x[...] *= numpy.random.rand(1)[0] * 10000
Minv = numpy.linalg.inv(M)
D = numpy.zeros((N, N))
for i in range(N):
    D[i, i] = i + 1
file = open('in/big.csv', 'w')
numpy.savetxt(file, Minv.dot(D).dot(M), delimiter=',', fmt='%.8f')
