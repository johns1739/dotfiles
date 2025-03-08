#!/bin/bash

# Install Brew
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

brew update

# Git
brew install git

# Btop
brew install btop

# Fzf
brew install fzf

# Ripgrep
brew install ripgrep

# Ghostty
brew install --cask ghostty

# Fonts
brew install --cask font-jetbrains-mono-nerd-font

# Emacs
brew tap d12frosted/emacs-plus
brew install emacs-plus
