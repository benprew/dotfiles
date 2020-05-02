if test -d ~/.rbenv
    set -g fish_user_paths $HOME/.rbenv/bin $HOME/.rbenv/shims $fish_user_paths
end

for f in ~/.gem/ruby/* do
    set bin_dir $f/bin/
    if test -d $bin_dir
        set -g fish_user_paths $bin_dir $fish_user_paths
    end
end
