PDIR=L
GHC_FLAGS=
#-XTypeSynonymInstances
HC=ghc --make $(GHC_FLAGS)

all : main Test

L/Par.y : $(PDIR)/L.cf
	echo "Launching bnfc"
	bnfc -d $^

L/Lex.x : L/Par.y
	echo "Doing nothing for Lex target"

L/Lex.hs : L/Lex.x
	cd $(PDIR) ; \
		alex -g Lex.x

L/Par.hs : L/Par.y
	cd $(PDIR) ; \
		happy -gca Par.y ; \

Test: L/Par.hs L/Lex.hs
	$(HC) $(PDIR)/Test.hs -o Test

main: L/Par.hs L/Lex.hs FORCE # let GHC decide all dependencies
	$(HC) Main.hs -o $@

runMain: main input.l
	./main input.l

runMainError: main input-error.l
	./main input-error.l

clean: FORCE
	-@rm -f *.log *.aux *.dvi
	-@rm -f *.hi L/*.hi LLanguage/*.hi
	-@rm -f *.o L/*.o LLanguage/*.o
	-@rm -f main
	-@rm -f Test

cleanBnfc: FORCE
	-@rm -f L/Lex.x L/Par.y
	-@rm -f L/*.hs

FORCE:
