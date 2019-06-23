Implementation of Postcript: small subset of Postscript language.
Program takes input in Postcript and transforms it to Postscript file.
One optional parameter N: zoom factor.

Example input:
```
0 0 moveto
0 10 10 mul lineto
```

Example output:
```
300 400 translate
0 0 moveto 0 100 lineto
stroke showpage
```

File `Mon.hs` is not mine.