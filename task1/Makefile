HC=ghc -O2
RM=rm -f

euler : Euler.hs
	$(HC) Euler.hs -o euler

build=Euler.hs mv.sh all.sh oneE.gp oneA.gp oneR.gp oneI.gp

28 : $(build)
	make euler
	./euler 28.0
	./mv.sh
	./all.sh 28.0

good : $(build)
	make euler
	./euler 0.5 0.9 5.0 14.0 15.0 24.0 28.0
	./mv.sh
