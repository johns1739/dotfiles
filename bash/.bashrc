PS1="[\w]\$ "

export EDITOR="emacs"
export SUDO_EDITOR="$EDITOR"

alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'

alias emacs="emacs-nox"
alias e="emacs"

# eza
alias ls='eza -lh --group-directories-first '
alias lsa='ls -a'
alias lt='eza --tree --level=2 --long --git'
alias lta='lt -a'

alias locate="plocate"
alias top="btop"
# bat
# tldr
# fzf
