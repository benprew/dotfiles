alias e 'emacsclient -n'

for module in (cat ~/.modules)
  set module_init_path $HOME/dotfiles/$module/init.fish
  if test -e $module_init_path
    . $module_init_path
  end

  if test -e $HOME/dotfiles/$module/bin
    set -x PATH $PATH $HOME/dotfiles/$module/bin
  end
end
set -g fish_user_paths "/usr/local/sbin" $fish_user_paths
