
all:
	cabal-dev install-deps --enable-test
	cabal-dev configure --enable-test
	cabal-dev build
