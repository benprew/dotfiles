if test -d ~/.rbenv
    set -g fish_user_paths $HOME/.rbenv/bin $HOME/.rbenv/shims $fish_user_paths
end

if test -d ~/.gem/ruby/2.5.0/bin/
    set -g fish_user_paths ~/.gem/ruby/2.5.0/bin/ $fish_user_paths
end

if test -d ~/.gem/ruby/2.7.0/bin/
    set -g fish_user_paths ~/.gem/ruby/2.7.0/bin/ $fish_user_paths
end
