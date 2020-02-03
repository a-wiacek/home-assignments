all:
	cd src &&\
	ghc --make LatteLLVM.hs -o latc_llvm &&\
	mv -f latc_llvm ../latc_llvm

clean:
	rm -f latc_llvm
	cd src &&\
	find . -type f -name '*.o' -delete &&\
    find . -type f -name '*.hi' -delete

