.PHONY: build

build:
	nix-shell --run 'hpack;cabal build -O0'
