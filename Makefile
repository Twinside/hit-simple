
all:
	runhaskell Setup.hs build

conf:
	runhaskell Setup.hs configure --user

install:
	runhaskell Setup.hs install

dist:
	runhaskell Setup.hs sdist

clean:
	runhaskell Setup.hs clean

doc:
	runhaskell Setup.hs haddock

HIT_VERSION:=0.3

pack:
	mkdir hit-simple-$(HIT_VERSION)
	cp -r Data hit-simple-$(HIT_VERSION)
	cp hit-simple.cabal hit-simple-$(HIT_VERSION)
	cp Setup.hs hit-simple-$(HIT_VERSION)
	cp README.md hit-simple-$(HIT_VERSION)
	cp LICENSE hit-simple-$(HIT_VERSION)
	tar cvf hit-simple-$(HIT_VERSION).tar hit-simple-$(HIT_VERSION)
	gzip hit-simple-$(HIT_VERSION).tar
	rm -Rf hit-simple-$(HIT_VERSION)

