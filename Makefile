all: build

.configured: shell.nix
	nix-shell --command 'cd src && cabal new-configure --enable-tests' --pure
	touch .configured
build: .configured
	nix-shell --command 'cd src && cabal new-build' --pure
repl: .configured
	nix-shell --command 'cd src && cabal new-repl' --pure
tests: .configured
	nix-shell --command 'cd src && cabal new-test' --pure
