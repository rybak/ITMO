HC=ghc -O9
RM=rm -f

build : fusion main

main : main.hs
	$(HC) main.hs -o main

rebuild : clean build

fusion : Fusion.hs
	$(HC) Fusion.hs -c

clean :
	$(RM) *.hi
	$(RM) *.o
	$(RM) main

fresh :
	ls | grep -E "\\.[0-9][0-9]$$" | xargs -t -I{} rm {}

calc=build all.sh oneEE.gp oneA.gp oneRK.gp oneIE.gp

28 : $(calc)
	make main
	./main 28.0
	./mv.sh
	./all.sh 28.0

good : $(calc)
	make main
	./main f 0.5 0.9 5.0 14.0 15.0 24.0 28.0

diff : $(calc)
	make main
	./main f 19.65
