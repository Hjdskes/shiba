executable:
	nix-build -A shiba.components.exes.shiba-scraper

library:
	nix-build -A shiba.components.library

lambda:
	nix-build ./lambda.nix

deploy:
	pushd ./tf ; \
	terraform apply ; \
	popd

.PHONY: executable library lambda deploy
