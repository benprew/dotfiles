# My Dotfiles

My approach to dotfiles as I've gotten older leans more minimalist. When I was
younger I spent time configmaxxing at the cost of getting real work down. With
advances in modern OSes and modern AI tooling this means that I can mostly use
things out of the box and spend less time fiddling with switches and config
settings.

I'm a heavy emacs user and most of my config is there. Even so, I try to use
features that are shipped in emacs and use external packages as a last resort.
Modern emacs has so many built-in features that it's quite capable.

1. Emacs - because it does just about everything
2. Fish - because it's focused on improving UX but I use bash for scripting
3. Git - I use git on all my projects but that may change in the future
4. Fedora Linux - it's FOSS!

## Installation

To get started:

```
$ git clone https://github.com/benprew/dotfiles.git
$ cd dotfiles
$ ./install.py setup_modules
$ <edit ~/.modules as appropriate>
$ ./install.py
```

The first run creates `~/.modules` from a template. Edit it to select which
modules to install, then run again to install them.

For a full install, files from every enabled module are linked first. Package
manifests are then combined into one Homebrew, apt, or dnf transaction. Module
install scripts run after the OS packages are available, followed by one Emacs
process that installs any missing tree-sitter grammars.

### Adding a New Module

Just make a directory, drop `init.fish` and/or `init.el` and/or
`<something>.symlink` into it, and list it in your modules file. (See Modules
section below for more info).

Normally, `module/config/app/settings.symlink` links to
`~/.config/app/settings`. For an OS-dependent destination, use a template variable
as the first directory below the module:

```text
unison/${unison_config_dir}/sync-hub.prf.symlink.tmpl
```

The directory name is literal in the repository. `template_context()` in
`install.py` supplies its absolute destination on the current system. This works
for both `.symlink` files and rendered `.symlink.tmpl` files. Files inside that
directory keep their relative paths, without the usual leading dot. Unknown
variables and relative destinations are rejected. Installation and removal use
the same mapping; removal uses the current OS and environment values.

Modules can declare dependencies with `Brewfile`, `apt-packages.txt`,
`fedora-packages.txt`, and `treesit-grammars.txt`. Tree-sitter declarations use
the following whitespace-separated format:

```
language repository [branch [source-directory]]
```

## Writing Code

This is where most of the magic happens, so follow along.

### Searching/Code Browsing

I spend almost all my time in emacs browsing/writing/editing code.  If
that's not your thing, there are other good packages built for that,
go look there.

I use project.el for project management. It allows you to find files in a project,
grep for strings and do whole project search/replace. A lot of the heavy lifting
is handled by LSPs.

Handy project.el shortcuts

```
<ctrl>-x-p-f - find file
<ctrl>-x-p-g - find string with grep
<ctrl>-. - go to definition
```

### Nagivating Windows

I use [Windmove](http://emacswiki.org/emacs/WindMove) for fast
naviation:
    `<alt>-<left>` - function: windmove-left
    `<alt>-<right>` - function: windmove-right
    `<alt>-<down>` - function: windmove-down
    `<alt>-<up>` - function: windmove-up

### Git For Source Control

Emacs has a lot of nice features for interacting with git,
but I also install [magit](http://www.emacswiki.org/emacs/Magit).

Common functions:

In a file:
    `<ctrl>-x-v-=` function: `vc-diff` - show changes to file
    `<ctrl>-x-v-D` function: `vc-root-diff` - show all changes in
    working set
    `<ctrl>-c-s` function: `magit-status`
    `<ctrl>-x-v-g` function: `vc-annotate` - git blame...

My dotfiles use eglot for managing LSPs and the module directories install the LSP
for the language and enable it in eglot (in init.el).

#### Modules

This is a fork of [@peterkeen](http://github.com/peterkeen)'s
[dotfiles](https://github.com/holman/dotfiles). The core idea is that these files
are organized by *topic*, not *software*. That means, for example, that all of the
ruby things, both bash and emacs, belong in the same area.

* Modules are listed explicitly in `~/.modules`, one per line, and are
  loaded in the specified order. This lets me set up different configs
  for work and home while still sharing almost everything else.
* Each module can have it's own `bin/`. These are added to the path
  module-file order.
* The core `fish` config looks for `init.fish` in every module
* The core `emacs` config looks for `init.el` in every module
