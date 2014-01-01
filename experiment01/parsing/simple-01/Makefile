
all : Parser.hs Main.hs 
	ghc -package parsec -o parse Parser.hs Main.hs

test : Parser.hs Test.hs
	ghc -package parsec -o test Parser.hs Test.hs

clean : 
	rm -rf parse test *.o *.hi
