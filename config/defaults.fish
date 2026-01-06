# Fish Shell Configuration

set -gx EDITOR "emacs"
set -gx SUDO_EDITOR "$EDITOR"
set -gx VISUAL "emacs"
set -gx GPG_TTY (tty)
set -gx GOPATH "$HOME/go"

fish_add_path "$GOPATH/bin"
fish_add_path "$HOME/.cargo/bin"
fish_add_path "$HOME/.local/bin"

if status --is-interactive
    direnv hook fish | source
    alias ....='cd ../../..'
    alias ...='cd ../..'
    alias ..='cd ..'
    alias e="emacs -nw"
    alias emacs="emacs -nw"
    alias grep="grep --line-buffered --color"
    alias l="less"
    alias lg="lazygit"
    alias ll="ls -lh --color"
    alias reload="source ~/.config/fish/config.fish"
    alias rgrep="grep --color -rnIi"
    alias top="btop"
end

# ASDF configuration code
if test -z $ASDF_DATA_DIR
    set _asdf_shims "$HOME/.asdf/shims"
else
    set _asdf_shims "$ASDF_DATA_DIR/shims"
end

# Do not use fish_add_path (added in Fish 3.2) because it
# potentially changes the order of items in PATH
if not contains $_asdf_shims $PATH
    set -gx --prepend PATH $_asdf_shims
end
set --erase _asdf_shims
