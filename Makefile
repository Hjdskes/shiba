format:
	nix-shell --pure --run "stylish-haskell -i -r ."

hlint:
	nix-shell --pure --run "hlint ."

executable:
	nix-build -A shiba.components.exes.shiba-scraper

library:
	nix-build -A shiba.components.library

test:
	nix-build -A shiba.components.tests.shiba-systest

lambda:
	nix-build ./lambda.nix

deploy:
	pushd ./tf ; \
	terraform apply ; \
	popd

.PHONY: executable library lambda deploy
