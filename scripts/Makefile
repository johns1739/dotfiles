.PHONY: deps install uninstall clean reinstall

janet:
	brew install janet

janet-lsp:
	jpm install https://github.com/CFiggers/janet-lsp

deps:
	jpm install sh

clean:
	rm -rf janet-lsp

install:
	jpm quickbin sync_repos ~/.local/bin/sync_repos

uninstall:
	rm -f 	~/.local/bin/sync_repos

reinstall: uninstall install
