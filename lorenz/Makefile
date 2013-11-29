HC=ghc -O9
RM=rm -f

build : fusion euler

euler : Euler.hs
	$(HC) Euler.hs -o euler

rebuild : clean build

fusion : Fusion.hs
	$(HC) Fusion.hs -c

clean :
	$(RM) *.hi
	$(RM) euler

calc=build mv.sh all.sh oneE.gp oneA.gp oneR.gp oneI.gp

28 : $(calc)
	make euler
	./euler 28.0
	./mv.sh
	./all.sh 28.0

good : $(calc)
	make euler
	./euler 0.5 0.9 5.0 14.0 15.0 24.0 28.0
	./mv.sh
