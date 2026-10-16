.PHONY: install-brew install-scripts install-janet-deps uninstall-scripts reinstall-scripts clean

BIN_DIR := ~/.local/bin

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

clean:
	rm -f $(BIN_DIR)/*.c
	rm -rf janet-lsp
	rm -rf build/
