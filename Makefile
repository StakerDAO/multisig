.PHONY: build

build:
	nix-shell --run 'hpack;cabal build -O0'

stylish:
	find app/ src/ -iname *.hs -exec stylish-haskell -i '{}' \;
