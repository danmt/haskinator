all: Haskinator

Haskinator:
	ghc -o Haskinator --make -main-is Haskinator Haskinator.hs
	rm -f *.o *.hi *~

clean:
	rm -f Haskinator *.o *.hi *~