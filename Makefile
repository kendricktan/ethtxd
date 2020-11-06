all: build

.configured: shell.nix
	nix-shell --command 'cd src && cabal v2-configure --enable-tests' --pure
	touch .configured
build: .configured
	nix-shell --command 'cd src && cabal v2-build' --pure
repl: .configured
	nix-shell --command 'cd src && cabal v2-repl' --pure
tests: .configured
	nix-shell --command 'cd src && cabal v2-test' --pure
