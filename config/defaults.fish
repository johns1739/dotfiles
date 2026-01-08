# Fish Shell Configuration

set fish_greeting # turn off welcome message

set -gx EDITOR "env SIMPLE=t emacs"
set -gx SUDO_EDITOR "$EDITOR"
set -gx VISUAL "emacs"
set -gx GPG_TTY (tty)

alias ....='cd ../../..'
alias ...='cd ../..'
alias ..='cd ..'
alias e="env SIMPLE=t emacs -nw"
alias emacs="emacs -nw"
alias grep="grep --line-buffered --color"
alias l="less"
alias lg="lazygit"
alias ll="ls -lh --color"
alias reload="source ~/.config/fish/config.fish"
alias rgrep="grep --color -rnIi"
alias top="btop"
