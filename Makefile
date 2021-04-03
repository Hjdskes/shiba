executable:
	nix-build -A shiba.components.exes.shiba-scraper

library:
	nix-build -A shiba.components.library

.PHONY: executable library
