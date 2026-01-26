# emacs all the dotfiles

This is my dotfile repo, there are many like it, but this one is
mine.  As with any dotfile repo, this one has opinions and will make
choices for you.

1. Emacs - because it does just about everything
2. Bash - because now that you're using emacs, you don't need anything
else
3. Git - I use git, on all my projects, personal and professional
4. OSX/Mac - most of the instructions are for Mac, but can easily be
adapted for Linux.
5. Brew - because it sucks less than ports, but that doesn't make it
good

In addition, there are some configs for Ruby and Ruby on Rails,
because I do most of my development in that language.

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

Available commands:
- `./install.py` or `./install.py install` - Install all modules
- `./install.py install_module <name>` - Install a specific module
- `./install.py uninstall` - Remove all symlinks (restores backups if present)
- `./install.py update` - Fetch updates from git origin
- `--symlinks-only` - Skip package installation and scripts, only create symlinks

Adding a new module is simple. Just make a directory, drop `init.sh`
and/or `init.el` and/or `<something>.symlink` into it, and list it in
your modules file. (See Modules section below for more info).

## Emacs

This is where most of the magic happens, so follow along.

### Searching/Code Browsing

I spend almost all my time in emacs browsing/writing/editing code.  If
that's not your thing, there are other good packages built for that,
go look there.

#### Finding Files

I use helm for finding files, it can be accessed by:
    `<ctrl>-c-h` - function: `helm-for-files`

I have helm setup to rember recently opened files, as well as setting
up a small locate database for finding files in your home
directory. See `core/init.el` for specific settings.

#### Method/Function Navigation

Tag expansion is built into emacs, but requires setting up ctags to
use it.  I have a hook script to regenerate the TAGS
file after doing a git operation (checkout, pull, etc)

    `<alt>-.` - function: `find-tag`

You'll need to install ctags (from brew) and add the file to your
hooks repo

    `brew install ctags`

#### Grep-ing Files

I use [Ag](https://github.com/ggreer/the_silver_searcher), which is like ack, but faster:
    `<ctrl>-c-g` - function: `ag-project-at-point`

This will use your current repos root as a starting point and search
from there.  If defaults to the string under the cursor.

If you want to do regexes over the project:

    `<ctrl>-c-G` - function: `ag-project-regexp`

To use Ag, you'll need to install it:

    `brew install the_silver_searcher`

### Nagivating Windows

I use [Windmove](http://emacswiki.org/emacs/WindMove) for fast
naviation:
    `<alt>-<left>` - function: windmove-left
    `<alt>-<right>` - function: windmove-right
    `<alt>-<down>` - function: windmove-down
    `<alt>-<up>` - function: windmove-up

### Git For Source Control

Emacs has a lot of nice features for interacting with git currently,
but I've also installed [magit](http://www.emacswiki.org/emacs/Magit)

Common functions:

In a file:
    `<ctrl>-x-v-=` function: `vc-diff` - show changes to file
    `<ctrl>-x-v-D` function: `vc-root-diff` - show all changes in
    working set
    `<ctrl>-c-s` function: `magit-status`
    `<ctrl>-x-v-g` function: `vc-annotate` - git blame...

### Writing Code

#### Automatic Syntax Check

You'll probably want to install a newer version of ruby locally
(assuming you're using a newer version in a vagrant instance or
similar)

`brew install ruby`

### Testing

I admit it, I write tests, and I use
[rinari](http://rinari.rubyforge.org/).  It doesn't find tests very
well in my current codebase, but I haven't taken the time to fix it.
    `<ctrl>-c-;-f-t` - function: `rinari-find-test`

We also use test through a vagrant instance, and use zeus to manage
that, so, when you're in a test file, you can say:
    `<ctrl>-c-t` - function: `kong-run-current-test-file`
    `<ctrl>-c-.` - function: `kong-run-test-at-point`

## Modules

This is a fork of [@peterkeen](http://github.com/peterkeen)'s
[dotfiles](https://github.com/holman/dotfiles). The core idea is that
these files are organized by *topic*, not *software*. That means, for
example, that all of the ruby things, both bash and emacs, belong in
the same area.

* Modules are listed explicitly in `~/.modules`, one per line, and are
  loaded in the specified order. This lets me set up different configs
  for work and home while still sharing almost everything else.
* Each module can have it's own `bin/`. These are added to the path
  module-file order.
* The core `bash` config looks for `init.sh` in every module
* The core `emacs` config looks for `init.el` in every module
* Emacs uses [el-get](https://github.com/dimitri/el-get) to manage 3rd
  party dependencies.
