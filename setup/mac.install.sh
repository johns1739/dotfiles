#!/bin/bash

# Brew
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
# Install homebrew when SUDO not allowed.
# mkdir homebrew && curl -L https://github.com/Homebrew/brew/tarball/master | tar xz --strip 1 -C homebrew
brew update

# https://github.com/junegunn/fzf/tree/master
brew install fzf

brew install git btop ripgrep
brew install --cask font-jetbrains-mono-nerd-font

# dotfiles
mkdir ~/.config
mkdir ~/workspaces
cd ~/workspaces && git clone https://github.com/johns1739/dotfiles.git

ln -s ~/workspaces/dotfiles/config/ghostty ~/.config/ghostty
brew install --cask ghostty

# Rust & Cargo
curl https://sh.rustup.rs -sSf | sh

# Emacs
ln -s ~/workspaces/dotfiles/config/emacs ~/.config/emacs
brew tap d12frosted/emacs-plus
brew install emacs-plus@31 \
     --with-modern-black-dragon-icon \
     --with-debug \
     --with-imagemagick \
     --with-mailutils \
     --with-no-frame-refocus
cargo install emacs-lsp-booster

# Dependencies
brew install asdf
brew install autoconf
brew install coreutils
brew install curl
brew install gcc
brew install jq
brew install libyaml
brew install multimarkdown
brew install openssl
brew install pkg-config
brew install readline
brew install wxwidgets
brew install zlib

brew install icu4c
brew link icu4c --force

# ASDF Helpful commands
# asdf list all <name>
# asdf reshim <name>

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
brew install python-tk
asdf install python latest

## Nodejs
asdf plugin add nodejs
asdf install nodejs latest

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
# psql postgres
# > create role juan superuser;
# > alter role juan with login;
# createdb juan
