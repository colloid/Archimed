ProgName = Main
Sources = Main.hs Parser.hs Lexer.hs Statement.hs Expression.hs Configuration.hs Ast.hs

all: $(ProgName)

$(ProgName): $(Sources)
	ghc -o $@ $<

Parser.hs: Parser.y
	happy $^

Lexer.hs: Lexer.x
	alex $^

clean:
	rm -f *.o *.hi Parser.hs Lexer.hs Main
