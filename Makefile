.PHONY: \
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
CONFIGS := claude emacs fish ghostty git psql
FOLDED_CONFIGS := emacs-snippets # To fold means to symlink to folder.

BREW_PACKAGES += autoconf     # Generates configure scripts for builds
BREW_PACKAGES += btop         # Interactive resource monitor
BREW_PACKAGES += direnv       # Loads per-directory environment variables
BREW_PACKAGES += fd           # Fast, user-friendly alternative to find
BREW_PACKAGES += fish         # Friendly interactive shell
BREW_PACKAGES += fzf          # Command-line fuzzy finder
BREW_PACKAGES += gcc          # GNU compiler collection
BREW_PACKAGES += gh           # GitHub CLI
BREW_PACKAGES += git          # Version control system
BREW_PACKAGES += git-delta    # Syntax-highlighting pager for git diffs
BREW_PACKAGES += gnupg        # GPG encryption and signing
BREW_PACKAGES += janet        # Janet programming language
BREW_PACKAGES += jq           # Command-line JSON processor
BREW_PACKAGES += libgccjit    # GCC JIT library for Emacs native-comp
BREW_PACKAGES += ripgrep      # Fast recursive grep
BREW_PACKAGES += stow         # Symlink farm manager for dotfiles
BREW_PACKAGES += tlrc         # tldr client for simplified man pages
BREW_PACKAGES += tree-sitter  # Incremental parser for syntax highlighting
BREW_PACKAGES += zoxide       # Smarter cd that learns frequent dirs

install-brew-packages:
	brew update
	brew install -v --quiet $(BREW_PACKAGES)

BREW_CASKS += font-jetbrains-mono-nerd-font  # JetBrains Mono patched with Nerd Font icons
BREW_CASKS += ghostty                        # GPU-accelerated terminal emulator

install-brew-cask:
	brew update
	brew install --cask $(BREW_CASKS)

install-brew-emacs:
	brew tap d12frosted/emacs-plus
	brew update
	brew install emacs-plus

install-scripts:
	jpm quickbin scripts/sync_repos $(BIN_DIR)/sync_repos
	jpm quickbin scripts/clean_repos $(BIN_DIR)/clean_repos
	jpm quickbin scripts/open_issues $(BIN_DIR)/open_issues
	rm -f $(BIN_DIR)/*.c

install-janet-deps:
	jpm install sh spork judge
	jpm install https://github.com/CFiggers/janet-lsp

install-cargo:
	curl https://sh.rustup.rs -sSf | sh

install-cargo-deps:
	cargo install emacs-lsp-booster

uninstall-scripts:
	rm -f $(BIN_DIR)/sync_repos
	rm -f $(BIN_DIR)/clean_repos
	rm -f $(BIN_DIR)/open_issues

reinstall-scripts: uninstall-scripts install-scripts

install-configs:
	stow --no-folding --dir configs --target ~ $(CONFIGS)
	stow --dir configs --target ~ $(FOLDED_CONFIGS)

uninstall-configs:
	stow --dir configs --target ~ --delete $(FOLDED_CONFIGS)
	stow --no-folding --dir configs --target ~ --delete $(CONFIGS)

default-shell-fish:
	grep -qxF "$$(command -v fish)" /etc/shells || command -v fish | sudo tee -a /etc/shells
	chsh -s "$$(command -v fish)"

test:
	judge scripts

test-accept:
	judge --accept scripts

clean:
	rm -f $(BIN_DIR)/*.c
	rm -rf janet-lsp
	rm -rf build/
