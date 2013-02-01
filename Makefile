
all:
	cabal-dev install-deps --enable-test
	cabal-dev configure --enable-test
	cabal-dev build

proto:
	rm -rf src/Network/Monitoring/Riemann/Proto/
	rm -f  src/Network/Monitoring/Riemann/Proto.hs
	hprotoc -p Network.Monitoring.Riemann -d src extra/riemann.proto 
