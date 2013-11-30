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

fresh :
	ls | grep -E "\\.[0-9][0-9]$$" | xargs -t -I{} rm {}

calc=build all.sh oneEE.gp oneA.gp oneRK.gp oneIE.gp

28 : $(calc)
	make euler
	./euler 28.0
	./mv.sh
	./all.sh 28.0

good : $(calc)
	make euler
	./euler f 0.5 0.9 5.0 14.0 15.0 24.0 28.0

diff : $(calc)
	make euler
	./euler f 19.65
