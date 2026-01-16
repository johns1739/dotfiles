# Fish Shell Configuration

set fish_greeting # turn off welcome message

set -gx EDITOR "env SIMPLE=t emacs -nw"
set -gx SUDO_EDITOR "$EDITOR"
set -gx VISUAL "$EDITOR"
set -gx GPG_TTY (tty)

fish_add_path "$HOME/.local/bin"

alias ....='cd ../../..'
alias ...='cd ../..'
alias ..='cd ..'
alias e="env SIMPLE=t emacs -nw"
alias emacs="emacs -nw"
alias grep="grep --line-buffered --color"
alias l="less"
alias lg="lazygit"
alias ll="ls -lh --color"
alias rgrep="grep --color -rnIi"
alias top="btop"
