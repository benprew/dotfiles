if test -d ~/.rbenv
    set -x PATH ~/.rbenv/shims $PATH
end

set -Ux fish_user_paths $HOME/.rbenv/bin $fish_user_paths
