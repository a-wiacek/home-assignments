all:
	cd src &&\
	bnfc Grammar.cf &&\
	happy -gca ParGrammar.y &&\
	alex -g LexGrammar.x &&\
	ghc --make Main.hs -o interpreter &&\
	mv -f interpreter ../interpreter

clean:
	rm -f interpreter
	cd src &&\
	rm -f *.hi *.o *.x *.y *.txt *.tex *.info *.bak &&\
	rm -f AbsGrammar.hs LexGrammar.hs ParGrammar.hs &&\
	rm -f PrintGrammar.hs SkelGrammar.hs TestGrammar.hs ErrM.hs

