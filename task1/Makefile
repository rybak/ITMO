HC=ghc -O2
RM=rm -f

build : Euler.hs
	$(HC) Euler.hs -o euler

clean :
	$(RM) *.hi
	$(RM) euler

calc=Euler.hs mv.sh all.sh oneE.gp oneA.gp oneR.gp oneI.gp

28 : $(calc)
	make euler
	./euler 28.0
	./mv.sh
	./all.sh 28.0

good : $(calc)
	make euler
	./euler 0.5 0.9 5.0 14.0 15.0 24.0 28.0
	./mv.sh
