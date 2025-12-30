# Fish Shell Configuration

set -gx EDITOR "emacs"
set -gx SUDO_EDITOR "$EDITOR"
set -gx VISUAL "emacs"
set -gx ASDF_DIR "$HOME/.asdf"
set -gx GPG_TTY (tty)
set -gx GOPATH "$HOME/go"

fish_add_path "$ASDF_DIR/shims"
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
