project_name = crypts-and-camls
opam_file = $(project_name).opam
DUNE = opam exec -- dune

help: ## Prints the commands in this make file
	@echo "List of available make commands";
	@echo "";
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "  \033[36m%-15s\033[0m %s\n", $$1, $$2}';
	@echo "";

all: clean app
# clean:
# 	rm -rf _build

default: build
	utop

lock: ## Generate a lock file
	opam lock -y .

build: ## Build the project
	@dune build @all

format: ## Format the code
	opam exec -- dune build @fmt --auto-promote

clean: ## Clean the project
	@dune clean
	rm -rf doc.public

docs: clean build
	@dune build @doc
	mkdir -p doc.public
	cp -r _build/default/_doc/_html doc.public

app:
	@dune exec ./dungeon.exe  --profile release

run-debug: ## Run the server with debug
	$(DUNE) exec $(project_name) -- --debug

install: ## Install the dependencies
	opam install . --deps-only --with-doc --with-test -y

.PHONY: init
init: ## Create a local opam switch and install deps
	# opam switch create . --locked
	opam switch create . 4.12.0 --deps-only --locked -y
	# opam install -y ocaml-lsp-server
	make install

hanoi: ## Build and run the Hanoi.ml project
	ocaml unix.cma Hanoi.ml

