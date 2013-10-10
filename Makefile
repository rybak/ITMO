all : main euler

main : main.hs
	ghc main.hs -o lorenz

euler : Euler.hs
	ghc Euler.hs -o euler
