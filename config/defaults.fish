# Fish Shell Configuration
set fish_greeting # turn off welcome message
bind --user alt-e true # unbind editor edit
bind --user alt-v true # unbind editor edit

set -gx EDITOR "emacs -nw"
set -gx SUDO_EDITOR "$EDITOR"
set -gx VISUAL "$EDITOR"
set -gx GPG_TTY (tty)

fish_add_path "$HOME/.local/bin"

alias ....='cd ../../..'
alias ...='cd ../..'
alias ..='cd ..'
alias em='emacsclient -nw -c -a ""'
alias emacs="emacs -nw"
alias emacs_server="emacsclient -e '(kill-emacs)' 2>/dev/null; emacs --daemon --debug-init 2>&1"
alias grep="grep --line-buffered --color"
alias l="less"
alias lg="lazygit"
alias ll="ls -lh --color"
alias rgrep="grep --color -rnIi"
alias top="btop"

if test "$INSIDE_EMACS" = 'vterm'
    and test -n "$EMACS_VTERM_PATH"
    and test -d "$EMACS_VTERM_PATH"
    source "$EMACS_VTERM_PATH/etc/emacs-vterm.fish"
end
