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

main: Main.hs L/Par.hs L/Lex.hs
	$(HC) $< -o $@

runMain: main input.l
	./main input.l

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi
	-rm main
	-rm Test
