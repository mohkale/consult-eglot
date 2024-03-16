SRC_DIR = .
BIN_DIR = bin

SRC      := $(wildcard $(SRC_DIR)/*.el)
ELC      := $(patsubst $(SRC_DIR)/%.el,$(BIN_DIR)/%.elc,$(SRC))
ELCHKDOC := $(patsubst $(SRC_DIR)/%.el,$(BIN_DIR)/%.checkdoc,$(SRC))

EMACS ?= cask emacs \
    --eval '(push (concat default-directory "./") load-path)'

$(V).SILENT:

.PHONY: ci/cd
ci/cd: lint

.PHONY: lint
lint: compile checkdoc

.PHONY: checkdoc
checkdoc: $(ELCHKDOC)

$(BIN_DIR)/%.checkdoc: $(SRC_DIR)/%.el
	mkdir -p "$$(dirname "$@")"
	@echo "[checkdoc] $^"
	$(EMACS) -Q --batch \
	    --eval "(or (fboundp 'checkdoc-file) (kill-emacs 1))" \
	    --eval "(setq sentence-end-double-space nil)" \
	    --eval "(checkdoc-file \"$^\")" 2>&1 \
		| tee "$@" \
	    | grep . && exit 1 || true

.PHONY: compile
compile: $(ELC)

$(BIN_DIR)/%.elc: $(SRC_DIR)/%.el
	mkdir -p "$$(dirname "$@")"
	@echo "[compile] $^"
	$(EMACS) -Q --batch -L . -f batch-byte-compile "$^" 2>&1 || exit 1
	mv -f "$^c" "$@"

.PHONY: clean
clean: ## Remove build artifacts
	for file in $(ELC) $(ELCHKDOC); do \
	    if [ -e "$$file" ]; then \
			echo "[clean] $$file"; \
	        rm -f "$$file"; \
	    fi; \
	done
