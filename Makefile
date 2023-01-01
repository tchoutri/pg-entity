deps: ## Install the dependencies of the backend
	@cabal build --only-dependencies

build: ## Build the project in fast mode
	@cabal build -O0

clean: ## Remove compilation artifacts
	@cabal clean

repl: ## Start a REPL
	@cabal repl

watch: ## Reload the code on change
	@ghcid

test: ## Run the test suite
	@cabal test

lint: ## Run the code linter (HLint)
	@find test src -name "*.hs" | xargs -P $(PROCS) -I {} hlint --refactor-options="-i" --refactor {}

style: ## Run the code styler (stylish-haskell)
	@find docs/src test src -name "*.hs" | xargs -P $(PROCS) -I {} fourmolu -q --mode inplace {}
	@cabal-fmt -i *.cabal

docs-build: ## Generate the documentation
	@cd docs; mkdocs build

docs-serve: ## Start a web server to serve the documentation
	@cd docs; mkdocs serve

help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.* ?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

PROCS := $(shell nproc)

.PHONY: all $(MAKECMDGOALS)

.DEFAULT_GOAL := help
