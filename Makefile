.PHONY: \
	install-brew \
	install-scripts \
	install-janet-deps \
	uninstall-scripts \
	reinstall-scripts \
	install-configs \
	uninstall-configs \
	test \
	test-accept \
	clean

BIN_DIR := ~/.local/bin
CONFIGS := claude emacs fish ghostty git psql

install-brew:
	brew install stow janet

install-scripts:
	jpm quickbin scripts/sync_repos $(BIN_DIR)/sync_repos
	jpm quickbin scripts/clean_repos $(BIN_DIR)/clean_repos
	jpm quickbin scripts/open_issues $(BIN_DIR)/open_issues
	rm -f $(BIN_DIR)/*.c

install-janet-deps:
	jpm install sh spork judge
	jpm install https://github.com/CFiggers/janet-lsp

uninstall-scripts:
	rm -f $(BIN_DIR)/sync_repos
	rm -f $(BIN_DIR)/clean_repos
	rm -f $(BIN_DIR)/open_issues

reinstall-scripts: uninstall-scripts install-scripts

install-configs:
	stow --dir configs --target ~ $(CONFIGS)

uninstall-configs:
	stow --dir configs --target ~ --delete $(CONFIGS)

test:
	judge scripts

test-accept:
	judge --accept scripts

clean:
	rm -f $(BIN_DIR)/*.c
	rm -rf janet-lsp
	rm -rf build/
