all:
	cd src &&\
	bnfc Instant.cf &&\
	happy -gca ParInstant.y &&\
	alex -g LexInstant.x &&\
	ghc --make InstantLLVM.hs -o insc_llvm &&\
	ghc --make InstantJVM.hs -o insc_jvm &&\
	mv -f insc_llvm ../insc_llvm &&\
	mv -f insc_jvm ../insc_jvm

clean:
	rm -f insc_llvm insc_jvm
	cd src &&\
	rm -f *.hi *.o *.x *.y *.txt *.tex *.info *.bak &&\
	rm -f AbsInstant.hs LexInstant.hs ParInstant.hs &&\
	rm -f PrintInstant.hs SkelInstant.hs TestInstant.hs ErrM.hs

