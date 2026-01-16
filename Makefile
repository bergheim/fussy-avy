EMACS ?= emacs

# Directory for test dependencies
DEPS_DIR := .deps

.PHONY: test clean deps

# Install dependencies (avy) for testing
deps: $(DEPS_DIR)/.installed

$(DEPS_DIR)/.installed:
	@mkdir -p $(DEPS_DIR)
	@$(EMACS) --batch \
		--eval "(require 'package)" \
		--eval "(setq package-user-dir \"$(CURDIR)/$(DEPS_DIR)\")" \
		--eval "(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\") t)" \
		--eval "(package-initialize)" \
		--eval "(package-refresh-contents)" \
		--eval "(package-install 'avy)"
	@touch $@

# Run tests
test: deps
	@$(EMACS) --batch \
		--eval "(setq package-user-dir \"$(CURDIR)/$(DEPS_DIR)\")" \
		--eval "(package-initialize)" \
		-L . \
		-l ert \
		-l fussy-avy.el \
		-l fussy-avy-test.el \
		-f ert-run-tests-batch-and-exit

# Clean up
clean:
	rm -rf $(DEPS_DIR)
