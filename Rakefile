# frozen_string_literal: true

require 'rake'
require 'pathname'

Dir.glob('*/*.rake').each { |r| import r }

@skip_all = false
@overwrite_all = false
@backup_all = false

task :setup_modules do
  modules_path = File.join(ENV['HOME'], '.modules')
  unless File.exist?(modules_path)
    `cp core/modules_template #{modules_path}`
    puts "Minimal modules template has been copied to #{modules_path}."
  end
end

desc 'Update the git repository from origin'
task :update do
  cur = File.dirname(__FILE__)
  `cd #{cur} && git fetch origin`
end

desc 'Hook our dotfiles into system-standard positions.'
task :install, [:symlinks_only] => :setup_modules do |_t, args|
  modules = File.read("#{ENV['HOME']}/.modules").split("\n")

  modules.each do |m|
    puts "==> Installing module: #{m}"
    install_module(m, args[:symlinks_only])
  end
end

desc 'Install dotfiles for a single module'
task :install_module, [:module_name] do |_task, args|
  module_name = args[:module_name]
  raise 'Please specify a module name to install (e.g., rake install_module[core])' if module_name.nil?

  puts "==> Installing dotfiles for module: #{module_name}"
  install_module(module_name)
end

task :uninstall do
  modules = File.read("#{ENV['HOME']}/.modules").split("\n")
  linkables = []
  modules.each do |m|
    linkables += Dir.glob(File.join(m, '*.symlink'))
  end

  linkables.each do |linkable|
    file = linkable.split('/').last.split('.symlink').last
    target = "#{ENV['HOME']}/.#{file}"

    # Remove all symlinks created during installation
    FileUtils.rm(target) if File.symlink?(target)

    # Replace any backups made during installation
    `mv "$HOME/.#{file}.backup" "$HOME/.#{file}"` if File.exist?("#{ENV['HOME']}/.#{file}.backup")
  end
end

def realpath(path)
  Pathname.new(path).realpath
rescue Errno::ENOENT
  nil
end

def install_script(install_script)
  return unless File.exist?(install_script)

  plat = install_script.match(/install_([^.]+)\.sh/)

  return unless install_script.match(/install\.sh/) || plat && RUBY_PLATFORM.match(plat[1])

  puts "\tRunning #{install_script}"
  `"#{install_script}"`
end

# linkable: full path to source file
# target: full back to target file
def mk_link(linkable, target)
  overwrite = false
  backup = false

  puts "\tinstalling #{linkable} to #{target}"
  tgt_realpath = realpath(target)
  lnk_realpth = realpath(linkable)

  return if File.symlink?(target) && tgt_realpath == lnk_realpth

  if File.exist?(target) || File.symlink?(target)
    unless @skip_all || @overwrite_all || @backup_all
      puts "File already exists: #{target}, what do you want to do? " \
           '[s]kip, [S]kip all, [o]verwrite, [O]verwrite all, [b]ackup, [B]ackup all'
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
    File.rename(target, "#{target}.backup") if File.exist?(target) && (backup || @backup_all)
  end
  FileUtils.mkdir_p(File.dirname(target))
  File.symlink(linkable, target)
end

def install_module(m, symlinks_only = false)
  raise("unknown module #{m}") unless Dir.exist?(m)

  linkables = Dir.glob(File.join(m, '/**/*.symlink'))
  linkables.each do |linkable|
    file = linkable.split('/')[1..-1].join('/').gsub('.symlink', '')
    target = "#{ENV['HOME']}/.#{file}"

    mk_link(File.join(Dir.pwd, linkable), target)
  end

  emacs_init = File.join(m, 'init.el')
  if File.exist?(emacs_init)
    src = emacs_init
    target_dir = "#{ENV['HOME']}/.emacs.d/personal/"
    FileUtils.mkdir_p target_dir
    target = "#{target_dir}/#{m}.el"
    mk_link(File.join(Dir.pwd, src), target)
  end

  return if symlinks_only

  if RUBY_PLATFORM.match('darwin')
    brewfile = "#{m}/Brewfile"
    if File.exist?(brewfile)
      puts "\tRunning #{brewfile}"
      puts `brew bundle --file="#{brewfile}" --no-lock`
    end
  elsif RUBY_PLATFORM.match('linux')
    package_providers = {
      apt: {
        install_cmd: 'sudo apt install -y',
        packages_file: 'apt-packages.txt'
      },
      dnf: {
        install_cmd: 'sudo dnf install -y',
        packages_file: 'fedora-packages.txt'
      },
    }

    pkg_provider = nil
    pkg_provider = :apt if system('command -v apt >/dev/null 2>&1')
    pkg_provider = :dnf if system('command -v dnf >/dev/null 2>&1')

    pkg_info = package_providers[pkg_provider]
    pkg_file = "#{m}/#{pkg_info[:packages_file]}"

    if pkg_provider && File.exist?(pkg_file)
      puts "\tInstalling packages in #{pkg_file} with #{pkg_provider}"
      puts 'Failed to install packages' unless system("cat \"#{pkg_file}\" | xargs #{pkg_info[:install_cmd]}")
    end
  end

  install_scripts = Dir.glob("#{m}/install*.sh")
  install_scripts.each { |s| install_script s }
end

task default: 'install'
