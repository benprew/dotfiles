set gem_bin_dir (ruby -r rubygems -e 'puts Gem.user_dir')/bin
add_path gem_bin_dir

if test -d ~/.rbenv
    add_path $HOME/.rbenv/bin
    add_path $HOME/.rbenv/shims
    add_path $HOME/.rbenv/plugins/ruby-build/bin
end


if command -s rbenv > /dev/null
    set -l rbenv_root ''
    if test -z "$RBENV_ROOT"
        set rbenv_root "$HOME/.rbenv"
        set -x RBENV_ROOT "$HOME/.rbenv"
    else
        set rbenv_root "$RBENV_ROOT"
    end

    set -x PATH $rbenv_root/shims $PATH
    set -x RBENV_SHELL fish
    if test ! -d "$rbenv_root/shims"; or test ! -d "$rbenv_root/versions"
        command mkdir -p $rbenv_root/{shims,versions}
    end
end
