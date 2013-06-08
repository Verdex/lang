
all : Parser.o Main.hs 
	ghc -package parsec -o parse Parser.o Main.hs

Parser.o : Parser.hs 
	ghc -package parsec -c Parser.hs

clean : 
	rm -rf parse *.o *.hi
