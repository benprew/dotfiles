if not test -f /etc/fedora-release
    set -l cargo_home "$HOME/.cargo"
    if set -q CARGO_HOME; and test -n "$CARGO_HOME"
        set cargo_home "$CARGO_HOME"
    end
    if test -r "$cargo_home/env.fish"
        source "$cargo_home/env.fish"
    end
end
