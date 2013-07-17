all: Main
	
Main: Main.hs Parser.hs Lexer.hs Statement.hs Expression.hs Configuration.hs
	ghc -o $@ $<

Parser.hs: Parser.y
	happy $^

Lexer.hs: Lexer.x
	alex $^

clean:
	rm -f *.o *.hi Parser.hs Lexer.hs Main
