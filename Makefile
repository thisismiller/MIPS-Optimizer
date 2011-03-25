pipeline: Lexer.hs Parser.hs Analysis.hs Main.hs
	ghc --make Main.hs -o $@

%.hs: %.x
	alex $^ -o $@

%.hs: %.y
	happy $^ -o $@

%.o: %.hs
	ghc -c $^ -o $@

clean:
	rm -f *.o *.hi pipeline Lexer.hs Parser.hs
