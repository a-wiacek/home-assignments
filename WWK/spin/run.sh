# Ogólna kompilacja (N < R)
spin -a -DR=3 -DN=2 robotnicy.pml
gcc -DMEMLIM=2048 -O2 -DXUSAFE -w -o pan pan.c

# Kompilacja dla przypadku 3a (N >= R)
mkdir 3a
cd 3a
cp ../robotnicy.pml robotnicy.pml
spin -a -DR=2 -DN=3 robotnicy.pml
gcc -DMEMLIM=2048 -O2 -DXUSAFE -w -o pan pan.c
cd ..

# Własność 1
pan -a -f -N q1

# Własność 2
pan -a -f -N q2

# Własność 3a (N >= R)
cd 3a
pan -a -f -N q3
cd ..

# Własność 3b (N < R)
pan -a -f -N q3

# Własność 4 (N < R)
pan -a -f -N q4

# Własność 5
pan -a -f -N q5

# Własność 6
pan -a -f -N q6
