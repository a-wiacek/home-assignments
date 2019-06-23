import numpy, scipy.linalg, sys, csv

program_usage_info = "Usage: " + sys.argv[0] + """ input_file k output_file (p):
input_file and output_file must be csv files,
k is number of eigenvalues to find,
p is optional parameter:
 * if p is 1 or none, eigenpairs are computed for matrix from input_file.
 * if p is -1, eigenpairs are computed for inverse matrix."""

def quit_program(message):
    print("Error: " + message)
    print(program_usage_info)
    quit()

# Find eigenvalue and eigenvector
# If mu is None, find largest eigenvalue
# If mu is a number, find eigenvalue closest to mu
def power_iteration(M, mu = None, epsilon = 10e-16):
    vector = numpy.random.rand(M.shape[1], 1)
    if mu:
        LU = scipy.linalg.lu_factor(M - mu * numpy.eye(M.shape[0]))
    prev_eigenvalue = 0.
    eigenvalue = 1.
    while abs(eigenvalue - prev_eigenvalue) > epsilon:
        if mu:
            vector = scipy.linalg.lu_solve(LU, vector)
        else:
            vector = M.dot(vector)
        vector /= numpy.linalg.norm(vector)
        prev_eigenvalue = eigenvalue
        eigenvalue = (vector.T.dot(M).dot(vector))[0, 0]
    return vector, eigenvalue

if __name__ == "__main__":
    if len(sys.argv) not in [4, 5]:
        quit_program("Invalid number of arguments")

    try:
        M = numpy.loadtxt(open(sys.argv[1], 'rb'), delimiter=',')
    except FileNotFoundError:
        quit_program("File " + sys.argv[1] + " does not exist")
    except:
        quit_program("Could not read matrix from " + sys.argv[1])
    if M.shape[0] != M.shape[1]:
        quit_program("Matrix is not square")
    n = M.shape[0]

    try:
        k = int(sys.argv[2])
    except ValueError:
        quit_program("Could not parse k: " + sys.argv[2])
    if k < 0:
        quit_program("Invalid value for k: " + str(k))
    if k > n:
        print("Warning: k = %d greater than matrix size n = %d" % (k, n))
        k = n

    try:
        save_file = open(sys.argv[3], 'w')
    except:
        quit_program("Could not create file " + sys.argv[3])

    inverse = False
    if len(sys.argv) == 5:
        if sys.argv[4] == "-1":
            inverse = True
        elif sys.argv[4] != "1":
            quit_program("Invalid value for p: " + sys.argv[4])

    eigenvectors_initial = []
    eigenvectors_final = numpy.empty([n, k])
    eigenvalues = []
    M_shift = 0.
    if inverse:
        # Shift spectrum of matrix so that all eigenvalues have same sign
        _, M_shift = power_iteration(M)
        M_shift += M_shift
        M = M + M_shift * numpy.eye(n)
    for i in range(k):
        if inverse:
            evector, evalue = power_iteration(M, M_shift)
        else:
            evector, evalue = power_iteration(M)
        eigenvalues.append(evalue)
        evector /= numpy.linalg.norm(evector)
        eigenvectors_initial.append(evector)
        M = M - evalue * evector.dot(evector.T)
        for j in range(i, 0, -1):
            evi = eigenvectors_initial[j - 1]
            evector = (evalue - eigenvalues[j - 1]) * evector + eigenvalues[j - 1] * evi.T.dot(evector) * evi
            evector /= numpy.linalg.norm(evector)
        eigenvectors_final[:, i] = evector.T

    if inverse:
        eigenvalues = [1 / (evalue - M_shift) for evalue in eigenvalues]
    for evalue in eigenvalues:
        print(evalue)
    numpy.savetxt(save_file, eigenvectors_final, delimiter=',', fmt='%.8f')
