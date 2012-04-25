
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
	mkdir hit-$(HIT_VERSION)
	cp -r Data hit-$(HIT_VERSION)
	cp hit.cabal hit-$(HIT_VERSION)
	cp Setup.hs hit-$(HIT_VERSION)
	cp README.md hit-$(HIT_VERSION)
	cp LICENSE hit-$(HIT_VERSION)
	tar cvf hit-$(HIT_VERSION).tar hit-$(HIT_VERSION)
	gzip hit-$(HIT_VERSION).tar
	rm -Rf hit-$(HIT_VERSION)

