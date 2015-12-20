PDIR=L
GHC_FLAGS=
#-XTypeSynonymInstances
HC=ghc --make $(GHC_FLAGS)

all: $(PDIR)/L.cf
	bnfc -d $^
	cd $(PDIR) ; \
		happy -gca Par.y ; \
		alex -g Lex.x
	$(HC) $(PDIR)/Test.hs -o Test

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi

main: Main.hs all
	$(HC) $< -o $@


runMain: main input.l
	./main input.l
