
all : Parser.hs Main.hs 
	ghc -package parsec -o parse Parser.hs Main.hs

clean : 
	rm -rf parse *.o *.hi
