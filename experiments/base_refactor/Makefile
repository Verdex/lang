
all : Parsing.o ParsingTest.o


Parsing.o : Parsing.hs
	ghc Parsing.hs

ParsingTest.o : Parsing.o ParsingTest.hs 
	ghc ParsingTest.hs

clean : 
	rm -rf *.o
	rm -rf *.hi
