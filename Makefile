pipeline: Parser.hs Analysis.hs Main.hs
	ghc --make Main.hs -o $@

%.o: %.hs
	ghc -c $^ -o $@

clean:
	rm -f *.o *.hi pipeline
