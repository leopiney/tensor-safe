build:
	# Creates .cabal definition file
	hpack

	# Initializes a sandbox environment
	cabal sandbox init

	# Builds tensor-safe
	cabal new-build

	# Installs tensor-safe
	cabal new-install                 \
		--executables exe:tensor-safe \
		--overwrite-policy=always     \
		--symlink-bindir ~/.local/bin/

	cabal new-install --lib lib:tensor-safe

clear:
	rm -rf                    \
		.cabal-sandbox        \
		.dist-newstyle        \
		.ghc.environment*     \
		.cabal.sandbox.config \
		tensor-safe.cabal
	
	cabal new-clean