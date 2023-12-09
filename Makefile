.PHONY: all clean clean-mid

all: prepare Latte

prepare: 
	rm -rf src/build
	mkdir src/build
	cd src/build && mkdir lang && cd lang && cp ../../latte.cf latte.cf && /home/students/inf/PUBLIC/MRJP/bin/bnfc --functor -m latte.cf
	cd src/build/lang && make && cp AbsLatte.hs LexLatte.hs ParLatte.hs ../../

Latte: src/AbsLatte.hs src/LexLatte.hs src/ParLatte.hs src/Frontend.hs src/Main.hs src/CFG.hs src/Compiler.hs src/PropagateConstants.hs
	ghc -isrc --make src/Main.hs -o src/build/Compiler
	cd src && rm -f *.o *.hi
	cp src/build/Compiler .
	mv Compiler latc
	chmod +x latc

clean:
	cd src && rm -f *.o *.hi && rm -f AbsLatte.hs LexLatte.hs ParLatte.hs 
	rm -rf src/build
	rm latc