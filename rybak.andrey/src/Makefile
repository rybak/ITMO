PDIR=L


all: $(PDIR)/L.cf
	bnfc -d $^
	cd $(PDIR) ; \
		happy -gca Par.y ; \
		alex -g Lex.x
	ghc --make $(PDIR)/Test.hs -o Test

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi

main: Main.hs all
	ghc --make $< -o $@


runMain: main input.l
	./main input.l
