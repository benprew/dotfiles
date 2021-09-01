# frozen_string_literal: true

require 'rake'
require 'pathname'

Dir.glob('*/*.rake').each { |r| import r }

task :setup_modules do
  modules_path = File.join(ENV['HOME'], ".modules")
  unless File.exist?(modules_path)
    `cp core/modules_template #{modules_path}`
    puts "Minimal modules template has been copied to #{modules_path}."
  end
end

desc "Update the git repository from origin"
task :update do
  cur = File.dirname(__FILE__)
  `cd #{cur} && git fetch origin`
end

def change_counts
  diff = `git rev-list --left-right --count origin/master...master`
  dirty = `git diff-files`.chomp.length
  left, right = diff.split("\t")
  left = left.to_i
  right = right.to_i

  return left, right, dirty
end

desc "Show differences between origin and master, loudly"
task :show_differences_loud do

  left, right, dirty = change_counts

  if right == 0 && left != 0
    puts "Dotfiles behind by #{left} commit(s) and can be fast forwarded"
  elsif right != 0 && left == 0
    puts "Dotfiles ahead by #{right} commit(s) and can be fast forwarded"
  elsif right != 0 && left != 0
    puts "Dotfiles diverged from master, by #{right} and #{left} respectively."
  end
  if dirty != 0
    puts "Dotfiles have uncommitted changes"
  end
end

desc "Show differences in a way that can be incorporated into a bash prompt"
task :show_differences_stat do
  left, right, dirty = change_counts

  stat = ""
  if left == 0 && right != 0
    stat = "+#{right}"
  elsif left != 0 && right == 0
    stat = "-#{left}"
  elsif left != 0 && right != 0
    stat = "#{left},#{right}"
  end

  stat = "**#{stat}" if dirty != 0

  puts "[#{stat}] " if stat != ""
end

def realpath(path)
  Pathname.new(path).realpath
rescue Errno::ENOENT
  nil
end

def install_script(install_script)
  return unless File.exist?(install_script)

  plat = install_script.match(/install_([^.]+)\.sh/)

  if install_script.match(/install\.sh/) || plat && RUBY_PLATFORM.match(plat[1])
    puts "\tRunning #{install_script}"
    `"#{install_script}"`
  end
end

def mk_link(linkable, target)
    overwrite = false
    backup = false

    puts "\tinstalling #{linkable} to #{target}"
    tgt_realpath = realpath(target)
    lnk_realpth = realpath("#{Dir.pwd}/#{linkable}")

    if File.symlink?(target) && tgt_realpath == lnk_realpth
      return
    end

    if File.exist?(target)
      unless @skip_all || @overwrite_all || @backup_all
        puts "File already exists: #{target}, what do you want to do? " \
             "[s]kip, [S]kip all, [o]verwrite, [O]verwrite all, [b]ackup, [B]ackup all"
        case STDIN.gets.chomp
        when 'o' then overwrite = true
        when 'b' then backup = true
        when 'O' then @overwrite_all = true
        when 'B' then @backup_all = true
        when 'S' then @skip_all = true
        when 's' then return
        end
      end
      if @skip_all
        puts 'Skipping...'
        return
      end
      FileUtils.rm_rf(target) if overwrite || @overwrite_all
      `mv "$HOME/.#{file}" "$HOME/.#{file}.backup"` if backup || @backup_all
    end
    `ln -s "$PWD/#{linkable}" "#{target}"`
end

desc 'Hook our dotfiles into system-standard positions.'
task :install, [:symlinks_only] => :setup_modules do |t, args|
  modules = File.read("#{ENV['HOME']}/.modules").split("\n")

  @skip_all = false
  @overwrite_all = false
  @backup_all = false
  modules.each do |m|
    puts "==> Installing module: #{m}"

    linkables = Dir.glob(File.join(m, '/**/*.symlink'))
    linkables.each do |linkable|
      file = linkable.split('/')[1..-1].join('/').gsub('.symlink', '')
      target = "#{ENV["HOME"]}/.#{file}"

      mk_link(linkable, target)
    end

    emacs_init = File.join(m, 'init.el')
    if File.exist?(emacs_init)
      src = emacs_init
      target_dir = "#{ENV['HOME']}/.emacs.d/personal/"
      FileUtils.mkdir_p target_dir
      target = "#{target_dir}/#{m}.el"
      mk_link(src, target)
    end

    next if args[:symlinks_only]

    if RUBY_PLATFORM.match('darwin')
      brewfile = "#{m}/Brewfile"
      if File.exist?(brewfile)
        puts "\tRunning #{brewfile}"
        puts `brew bundle --file="#{brewfile}" --no-lock`
      end
    elsif RUBY_PLATFORM.match('linux')
      pkg_file = "#{m}/apt-packages.txt"
      if File.exist?(pkg_file)
        puts "\tInstalling packages in #{pkg_file}"
        `cat "#{pkg_file}" | xargs sudo apt-get install -y`
      end
    end

    install_scripts = Dir.glob("#{m}/install*.sh")
    install_scripts.each { |s| install_script s  }
  end
end

task :uninstall do
  modules = File.read("#{ENV['HOME']}/.modules").split("\n")
  linkables = []
  modules.each do |m|
    linkables += Dir.glob(File.join(m, "*.symlink"))
  end

  linkables.each do |linkable|

    file = linkable.split('/').last.split('.symlink').last
    target = "#{ENV["HOME"]}/.#{file}"

    # Remove all symlinks created during installation
    if File.symlink?(target)
      FileUtils.rm(target)
    end

    # Replace any backups made during installation
    if File.exists?("#{ENV["HOME"]}/.#{file}.backup")
      `mv "$HOME/.#{file}.backup" "$HOME/.#{file}"`
    end

  end
end

task :default => 'install'
