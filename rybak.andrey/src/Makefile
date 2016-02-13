PDIR=L
GHC_FLAGS=
#-XTypeSynonymInstances
HC=ghc --make $(GHC_FLAGS)

all : main Test

L/Par.y : $(PDIR)/L.cf
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

main: FORCE # let GHC decide all dependencies
	$(HC) Main.hs -o $@

runMain: main input.l
	./main input.l

clean: FORCE
	-rm -f *.log *.aux *.dvi
	-rn -f *.hi L/*.hi LLanguage/*.hi
	-rm -f *.o L/*.o LLanguage/*.o
	-rm main
	-rm Test

cleanBnfc: FORCE
	-rm L/Lex.x L/Par.y
	-rm L/*.hs

FORCE:
