all: get-password

.PHONY=test
test:
	@cabal test --test-show-details=direct --test-option=--format=checks

get-password: get-password.cabal $(shell find {src,app} -type f)
	@cabal build
	@cp $$(cabal list-bin get-password) get-password

.PHONY=clean
clean:
	@cabal clean
	@rm -f get-password

.PHONY=install-tools
install-tools:
	@cabal install hlint ormolu

.PHONY=lint
lint:
	@find {src,app,tests} -type f -name '*.hs' -exec hlint {} \;
	@find {src,app,tests} -type f -name '*.hs' -exec ormolu --mode check {} \;

.PHONY=format
format:
	@find {src,app,tests} -type f -name '*.hs' -exec ormolu --mode inplace {} \;
	