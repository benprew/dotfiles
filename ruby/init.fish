# for f in ~/.gem/ruby/* do
#     set bin_dir $f/bin/
#     if test -d $bin_dir
#         set -g fish_user_paths $bin_dir $fish_user_paths
#     end
# end

if test -d ~/.rbenv
    set -g fish_user_paths $HOME/.rbenv/bin $HOME/.rbenv/shims $fish_user_paths
    set -g fish_user_paths $HOME/.rbenv/plugins/ruby-build/bin $fish_user_paths
end


if not command -s rbenv > /dev/null
    echo "rbenv: command not found. See https://github.com/rbenv/rbenv"
    exit 1
end

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
