#!/bin/bash

# Brew
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

# Fonts
brew install --cask font-jetbrains-mono-nerd-font

# Ghostty
brew install --cask ghostty

# Emacs
brew tap d12frosted/emacs-plus
brew install emacs-plus

# Neovim
brew install neovim

# Dependencies
brew install autoconf
brew install openssl
brew install wxwidgets
brew install libxslt fop
brew install libyaml

# Asdf
brew install asdf

# Elixir
asdf plugin add elixir https://github.com/asdf-vm/asdf-elixir.git
asdf install elixir latest

# Erlang
asdf plugin add erlang https://github.com/asdf-vm/asdf-erlang.git
KERL_BUILD_DOCS=yes
KERL_INSTALL_HTMLDOCS=yes
KERL_INSTALL_MANPAGES=yes
asdf install erlang latest

# Ruby
asdf plugin add ruby https://github.com/asdf-vm/asdf-ruby.git
asdf install ruby latest

# Python
asdf plugin add python
asdf install python latest
