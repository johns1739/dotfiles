#!/bin/bash

# Brew
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
# Install homebrew when SUDO not allowed.
# mkdir homebrew && curl -L https://github.com/Homebrew/brew/tarball/master | tar xz --strip 1 -C homebrew
brew update

# Git
brew install git

# AWS
brew install awscli

# Btop
brew install btop

# Fzf
brew install fzf

# Ripgrep
brew install ripgrep

# GPG
brew install gnupg

# Fonts
brew install --cask font-jetbrains-mono-nerd-font

# Ghostty
brew install --cask ghostty

# Emacs
brew tap d12frosted/emacs-plus
brew install emacs-plus@31 \
     --with-modern-black-dragon-icon \
     --with-debug \
     --with-imagemagick \
     --with-mailutils \
     --with-no-frame-refocus

# Neovim
brew install neovim

# Helix
brew install helix

# Dependencies
brew install autoconf
brew install coreutils
brew install crypto
brew install curl
brew install gcc
brew install guile
brew install icu4c
brew install jq
brew install libxslt fop
brew install libyaml
brew install openssl
brew install ossp-uuid
brew install pkg-config
brew install readline
brew install wxwidgets
brew install unixodbc
brew install zlib

# Docker
brew install --cask docker

# Linking
brew link icu4c --force

# Asdf
brew install asdf

## Elixir
asdf plugin add elixir https://github.com/asdf-vm/asdf-elixir.git
asdf install elixir latest

## Erlang
asdf plugin add erlang https://github.com/asdf-vm/asdf-erlang.git
KERL_BUILD_DOCS=yes
KERL_INSTALL_HTMLDOCS=yes
KERL_INSTALL_MANPAGES=yes
asdf install erlang latest

## Ruby
asdf plugin add ruby https://github.com/asdf-vm/asdf-ruby.git
asdf install ruby latest
# gem install rails
# gem install solargraph
# gem install rubocop

## Python
asdf plugin add python
asdf install python latest

## Golang
asdf plugin add golang https://github.com/asdf-community/asdf-golang.git
asdf install golang latest
go install -v golang.org/x/tools/gopls@latest
asdf reshim golang

## Zig
asdf plugin add zig https://github.com/asdf-community/asdf-zig.git
asdf install zig latest

## Postgres - many openssl issues, probably better to download pgadmin
asdf plugin add postgres
asdf install postgres latest
# asdf set -u postgres 17.5
# pg_ctl start/stop
# psql -h localhost -U postgres
# > create role juan superuser;
# > alter role juan with login;
# createdb juan
