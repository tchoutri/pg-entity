deps: ## Install the dependencies of the backend
	@cabal build --only-dependencies

build: ## Build the project in fast mode
	@cd docs && mdbook build
	@cabal build -j

clean: ## Remove compilation artifacts
	@cabal clean

repl: ## Start a REPL
	@cabal repl

watch: ## Reload the code on change
	@ghcid

test: ## Run the test suite
	@./scripts/run-tests.sh

lint: ## Run the code linter (HLint)
	@find test src -name "*.hs" | xargs -P $(PROCS) -I {} hlint --refactor-options="-i" --refactor {}

style: ## Run the code styler (stylish-haskell)
	@find docs/src test src -name "*.hs" | xargs -P $(PROCS) -I {} fourmolu -q --mode inplace {}
	@cabal-fmt -i *.cabal

docs-build: ## Generate the documentation
	@cd docs; mkdocs build

docs-serve: ## Start a web server to serve the documentation
	@cd docs; mkdocs serve

db-create: ## Create the database
	@createdb -h $(DB_HOST) -p $(DB_PORT) -U $(DB_USER) $(DB_DATABASE)

db-drop: ## Drop the database
	@dropdb -f --if-exists -h $(DB_HOST) -p $(DB_PORT) -U $(DB_USER) $(DB_DATABASE)

db-setup: db-create db-init db-migrate ## Setup the dev database

db-init: ## Create the database schema
	@migrate init "$(DB_CONNSTRING)" 

db-migrate: ## Apply database migrations
	@migrate migrate "$(DB_CONNSTRING)" test/migrations

db-reset: db-drop db-setup db-provision ## Reset the dev database

help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.* ?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

PROCS := $(shell nproc)

.PHONY: all $(MAKECMDGOALS)

.DEFAULT_GOAL := help
