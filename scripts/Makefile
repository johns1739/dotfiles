.PHONY: janet janet-lsp deps install uninstall clean reinstall

janet:
	brew install janet

janet-lsp:
	jpm install https://github.com/CFiggers/janet-lsp

deps:
	jpm install sh

clean:
	rm -rf janet-lsp
	rm -rf build/

install:
	jpm quickbin sync_repos ~/.local/bin/sync_repos
	jpm quickbin clean_repos ~/.local/bin/clean_repos

uninstall:
	rm -f 	~/.local/bin/sync_repos
	rm -f 	~/.local/bin/clean_repos

reinstall: uninstall install clean
