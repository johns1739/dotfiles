.PHONY: \
	install-apt-packages \
	install-brew-packages \
	install-brew-cask \
	install-brew-emacs \
	install-scripts \
	install-janet-deps \
	install-cargo \
	install-cargo-deps \
	uninstall-scripts \
	reinstall-scripts \
	install-configs \
	uninstall-configs \
	default-shell-fish \
	test \
	test-accept \
	clean

BIN_DIR := ~/.local/bin

PACKAGES += autoconf			 # Generates configure scripts for builds
PACKAGES += btop				 # Interactive resource monitor
PACKAGES += direnv				 # Loads per-directory environment variables
BREW_PACKAGES += fd				 # Fast, user-friendly alternative to find
APT_PACKAGES += fd-find          # Fast, user-friendly alternative to find
PACKAGES += ffmpeg				 # Emacs package dependency for tmr
PACKAGES += fish				 # Friendly interactive shell
PACKAGES += fzf					 # Command-line fuzzy finder
PACKAGES += gcc					 # GNU compiler collection
PACKAGES += gh					 # GitHub CLI
PACKAGES += git					 # Version control system
PACKAGES += git-delta			 # Syntax-highlighting pager for git diffs
PACKAGES += gnupg				 # GPG encryption and signing
BREW_PACKAGES += janet			 # Janet programming language
# APT_PACKAGES += janet			 # Janet programming language
PACKAGES += jq					 # Command-line JSON processor
BREW_PACKAGES += libgccjit		 # GCC JIT library for Emacs native-comp
# APT_PACKAGES += libgccjit		 # GCC JIT library for Emacs native-comp
PACKAGES += ripgrep				 # Fast recursive grep
PACKAGES += stow				 # Symlink farm manager for dotfiles
BREW_PACKAGES += tlrc			 # tldr client for simplified man pages
# APT_PACKAGES += tlrc			 # tldr client for simplified man pages
BREW_PACKAGES += tree-sitter	 # Incremental parser for syntax highlighting
APT_PACKAGES += tree-sitter-cli  # Incremental parser for syntax highlighting
PACKAGES += zoxide				 # Smarter cd that learns frequent dirs

install-brew-packages:
	brew update
	brew install -yv --quiet $(BREW_PACKAGES) $(PACKAGES)

BREW_CASKS += font-jetbrains-mono-nerd-font  # JetBrains Mono patched with Nerd Font icons
BREW_CASKS += ghostty                        # GPU-accelerated terminal emulator

install-brew-cask:
	brew update
	brew install --cask $(BREW_CASKS)

install-brew-emacs:
	brew tap d12frosted/emacs-plus
	brew update
	brew install -y emacs-plus

install-apt-packages:
	apt update
	apt install -y $(APT_PACKAGES) $(PACKAGES)

install-apt-ghostty:
	/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/mkasberg/ghostty-ubuntu/HEAD/install.sh)"

install-cargo:
	curl https://sh.rustup.rs -sSf | sh

install-cargo-deps:
	cargo install emacs-lsp-booster

CONFIGS := claude emacs fish ghostty git psql
FOLDED_CONFIGS := emacs-snippets # To fold means to symlink to folder.

install-configs:
	stow --no-folding --dir configs --target ~ $(CONFIGS)
	stow --dir configs --target ~ $(FOLDED_CONFIGS)

uninstall-configs:
	stow --dir configs --target ~ --delete $(FOLDED_CONFIGS)
	stow --no-folding --dir configs --target ~ --delete $(CONFIGS)

default-shell-fish:
	grep -qxF "$$(command -v fish)" /etc/shells || command -v fish | sudo tee -a /etc/shells
	chsh -s "$$(command -v fish)"

install-janet-deps:
	jpm install sh spork judge
	jpm install https://github.com/CFiggers/janet-lsp

install-scripts:
	jpm quickbin scripts/sync_repos $(BIN_DIR)/sync_repos
	jpm quickbin scripts/clean_repos $(BIN_DIR)/clean_repos
	jpm quickbin scripts/open_issues $(BIN_DIR)/open_issues
	rm -f $(BIN_DIR)/*.c

uninstall-scripts:
	rm -f $(BIN_DIR)/sync_repos
	rm -f $(BIN_DIR)/clean_repos
	rm -f $(BIN_DIR)/open_issues

reinstall-scripts: uninstall-scripts install-scripts

test:
	judge scripts

test-accept:
	judge --accept scripts

clean:
	rm -f $(BIN_DIR)/*.c
	rm -rf janet-lsp
	rm -rf build/
