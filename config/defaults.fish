# Fish Shell Configuration
set fish_greeting # turn off welcome message
bind --user alt-e true # unbind editor edit
bind --user alt-v true # unbind editor edit

# TODO: Add e & emacsnw scripts
fish_add_path "$HOME/.local/bin"

set -gx EDITOR e
set -gx SUDO_EDITOR emacsnw
set -gx VISUAL e
set -gx GPG_TTY (tty)

if status is-interactive
    alias ....='cd ../../..'
    alias ...='cd ../..'
    alias ..='cd ..'
    alias grep="grep --line-buffered --color"
    alias l="less"
    alias lg="lazygit"
    alias ll="ls -lh --color"
    alias rgrep="grep --color -rnIi"
    alias top="btop"

    fzf --fish | source
    alias ff="fzf"

    zoxide init fish --cmd cd | source
end

if test "$INSIDE_EMACS" = vterm
    and test -n "$EMACS_VTERM_PATH"
    and test -d "$EMACS_VTERM_PATH"
    source "$EMACS_VTERM_PATH/etc/emacs-vterm.fish"

    function ff
        set -q argv[1]; or set argv[1] "."
        vterm_cmd find-file (realpath "$argv")
    end

    function say
        vterm_cmd message "%s" "$argv"
    end
end

if string match -qr '^ghostel(,|$)' -- "$INSIDE_EMACS"
    and test -n "$EMACS_GHOSTEL_PATH"
    and test -d "$EMACS_GHOSTEL_PATH"
    source "$EMACS_GHOSTEL_PATH/etc/shell/ghostel.fish"

    function ff
        set -q argv[1]; or set argv[1] "."
        ghostel_cmd find-file-other-window "$argv"
    end
end
