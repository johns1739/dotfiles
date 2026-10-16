.PHONY: janet janet-lsp deps install uninstall clean reinstall

BIN_DIR := ~/.local/bin

janet:
	brew install janet

janet-lsp:
	jpm install https://github.com/CFiggers/janet-lsp

deps:
	jpm install sh spork

clean:
	rm -rf janet-lsp
	rm -rf build/

install:
	jpm quickbin scripts/sync_repos $(BIN_DIR)/sync_repos
	jpm quickbin scripts/clean_repos $(BIN_DIR)/clean_repos
	jpm quickbin scripts/open_issues $(BIN_DIR)/open_issues

uninstall:
	rm -f $(BIN_DIR)/sync_repos
	rm -f $(BIN_DIR)/clean_repos
	rm -f $(BIN_DIR)/open_issues

reinstall:uninstall install clean
