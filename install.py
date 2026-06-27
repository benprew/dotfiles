#!/usr/bin/env python3
"""
Dotfiles installation script.

Usage:
    ./install.py                    # Install all modules
    ./install.py install            # Install all modules
    ./install.py install_module <module_name>  # Install specific module
    ./install.py setup_modules      # Create ~/.modules from template
    ./install.py uninstall          # Uninstall all symlinks
    ./install.py update             # Update from git origin
"""

import argparse
import os
import platform
import shutil
import string
import subprocess
import sys
from pathlib import Path


class DotfilesInstaller:
    def __init__(self):
        self.skip_all = False
        self.overwrite_all = False
        self.backup_all = False
        self.home = Path.home()
        self.dotfiles_dir = Path(__file__).parent.resolve()
        self.modules_file = self.home / '.modules'

    def setup_modules(self):
        """Create .modules file from template if it doesn't exist."""
        if not self.modules_file.exists():
            template = self.dotfiles_dir / 'core' / 'modules_template'
            shutil.copy(template, self.modules_file)
            print(f"Minimal modules template has been copied to {self.modules_file}.")

    def get_modules(self):
        """Read and return list of modules from .modules file."""
        if not self.modules_file.exists():
            print("Error: .modules file not found. Run setup first.")
            sys.exit(1)

        with open(self.modules_file, 'r') as f:
            return [line.strip() for line in f if line.strip()]

    def realpath(self, path):
        """Get real path, returning None if path doesn't exist."""
        try:
            return Path(path).resolve(strict=True)
        except (OSError, RuntimeError):
            return None

    def _handle_existing(self, target_path):
        """Resolve an existing target via prompts and prepare its parent dir.

        Returns False if the caller should skip this target, True to proceed
        with creating the link/file.
        """
        overwrite = False
        backup = False

        if target_path.exists() or target_path.is_symlink():
            if not (self.skip_all or self.overwrite_all or self.backup_all):
                print(f"File already exists: {target_path}, what do you want to do?")
                print("[s]kip, [S]kip all, [o]verwrite, [O]verwrite all, [b]ackup, [B]ackup all")

                choice = input().strip().lower()

                if choice == 'o':
                    overwrite = True
                elif choice == 'b':
                    backup = True
                elif choice == 'O':
                    self.overwrite_all = True
                elif choice == 'B':
                    self.backup_all = True
                elif choice == 'S':
                    self.skip_all = True
                elif choice == 's':
                    return False

            if self.skip_all:
                print('Skipping...')
                return False

            if overwrite or self.overwrite_all:
                if target_path.is_dir() and not target_path.is_symlink():
                    shutil.rmtree(target_path)
                else:
                    target_path.unlink(missing_ok=True)

            if target_path.exists() and (backup or self.backup_all):
                backup_path = Path(str(target_path) + '.backup')
                target_path.rename(backup_path)

        # Create parent directories if needed
        target_path.parent.mkdir(parents=True, exist_ok=True)
        return True

    def mk_link(self, linkable, target):
        """Create a symlink, handling existing files with user prompts."""
        print(f"\tinstalling {linkable} to {target}")

        target_path = Path(target)
        linkable_path = Path(linkable)

        # Skip if already correctly linked
        if target_path.is_symlink() and self.realpath(target) == self.realpath(linkable):
            return

        if not self._handle_existing(target_path):
            return

        target_path.symlink_to(linkable_path)

    def template_context(self):
        """Variables exposed to *.symlink.tmpl templates, derived from OS/ARCH."""
        machine = platform.machine()
        is_mac = sys.platform == 'darwin'
        is_arm = machine in ('aarch64', 'arm64', 'armv7l', 'arm')

        # Prefix for launching x86_64 binaries: aarch64 Linux (e.g. Asahi) runs
        # them under muvm+fex; native x86 and macOS run them directly.
        x86_exec_prefix = 'muvm --emu=fex ' if (not is_mac and is_arm) else ''

        return {
            'os': 'darwin' if is_mac else ('linux' if sys.platform.startswith('linux') else sys.platform),
            'arch': machine,
            'x86_exec_prefix': x86_exec_prefix,
            # App paths that differ per OS live here so the conditional stays in
            # one place; templates just reference @@p4merge_path.
            'p4merge_path': ('/Applications/p4merge.app/Contents/MacOS/p4merge'
                             if is_mac else '/opt/p4v/bin/p4merge'),
        }

    def install_template(self, linkable, target):
        """Render a *.symlink.tmpl file and write it as a real file at target.

        Placeholders use string.Template's ``$name`` / ``${name}`` syntax.
        safe_substitute only replaces names present in template_context(), so
        unrelated shell tokens like ``$merge_tool_path`` or ``$(realpath ...)``
        in the source pass through untouched. (Note: ``$$`` collapses to ``$``,
        so keep context keys distinctively named to avoid shadowing literals.)
        """
        target_path = Path(target)
        rendered = string.Template(Path(linkable).read_text()).safe_substitute(
            self.template_context())

        # Already rendered and up to date: nothing to do (avoids re-prompting).
        if (target_path.is_file() and not target_path.is_symlink()
                and target_path.read_text() == rendered):
            return

        print(f"\tinstalling template {linkable} to {target}")
        if not self._handle_existing(target_path):
            return

        target_path.write_text(rendered)

    def install_script(self, install_script):
        """Run an install script if it matches the current platform."""
        script_path = Path(install_script)
        if not script_path.exists():
            return

        script_name = script_path.name

        # Check if it's a generic install.sh or platform-specific
        if script_name == 'install.sh':
            should_run = True
        else:
            # Check for platform-specific scripts (e.g., install_linux.sh)
            import platform
            plat = script_name.replace('install_', '').replace('.sh', '')
            should_run = sys.platform.startswith(plat.lower())

        if should_run:
            print(f"\tRunning {install_script}")
            subprocess.run([str(script_path)], check=False)

    def install_module(self, module_name, symlinks_only=False):
        """Install a single module's dotfiles and packages."""
        module_path = self.dotfiles_dir / module_name

        if not module_path.is_dir():
            raise ValueError(f"Unknown module: {module_name}")

        # Install symlinks (skip *.symlink.tmpl, handled as templates below)
        linkables = [p for p in module_path.rglob('*.symlink')
                     if not p.name.endswith('.symlink.tmpl')]
        for linkable in linkables:
            # Get relative path from module directory
            rel_path = linkable.relative_to(module_path)
            # Remove the module name and .symlink extension
            file_path = str(rel_path).replace('.symlink', '')
            target = self.home / f'.{file_path}'

            self.mk_link(linkable, target)

        # Render templates (*.symlink.tmpl) into real files based on OS/ARCH
        for tmpl in module_path.rglob('*.symlink.tmpl'):
            rel_path = tmpl.relative_to(module_path)
            file_path = str(rel_path).replace('.symlink.tmpl', '')
            target = self.home / f'.{file_path}'

            self.install_template(tmpl, target)

        # Handle emacs init files
        emacs_init = module_path / 'init.el'
        if emacs_init.exists():
            target_dir = self.home / '.emacs.d' / 'personal'
            target_dir.mkdir(parents=True, exist_ok=True)
            target = target_dir / f'{module_name}.el'
            self.mk_link(emacs_init, target)

        if symlinks_only:
            return

        # Install packages based on platform
        if sys.platform == 'darwin':
            # macOS - use Homebrew
            brewfile = module_path / 'Brewfile'
            if brewfile.exists():
                print(f"\tRunning {brewfile}")
                subprocess.run(['brew', 'bundle', f'--file={brewfile}'],
                             check=False)
        elif sys.platform.startswith('linux'):
            # Linux - detect package manager
            package_providers = {
                'apt': {
                    'install_cmd': 'sudo apt install -y',
                    'packages_file': 'apt-packages.txt'
                },
                'dnf': {
                    'install_cmd': 'sudo dnf install -y',
                    'packages_file': 'fedora-packages.txt'
                },
            }

            pkg_provider = None
            if shutil.which('apt'):
                pkg_provider = 'apt'
            elif shutil.which('dnf'):
                pkg_provider = 'dnf'

            if pkg_provider:
                pkg_info = package_providers[pkg_provider]
                pkg_file = module_path / pkg_info['packages_file']

                if pkg_file.exists():
                    print(f"\tInstalling packages in {pkg_file} with {pkg_provider}")
                    with open(pkg_file, 'r') as f:
                        packages = f.read().strip().split()

                    if packages:
                        cmd = pkg_info['install_cmd'].split() + packages
                        result = subprocess.run(cmd, check=False)
                        if result.returncode != 0:
                            print('Failed to install packages')

        # Run install scripts
        install_scripts = list(module_path.glob('install*.sh'))
        for script in install_scripts:
            self.install_script(script)

    def install(self, symlinks_only=False):
        """Install all modules listed in .modules file."""
        self.setup_modules()
        modules = self.get_modules()

        for module in modules:
            print(f"==> Installing module: {module}")
            self.install_module(module, symlinks_only)

    def uninstall(self):
        """Remove all symlinks created by the installer."""
        modules = self.get_modules()
        linkables = []
        templates = []

        for module in modules:
            module_path = self.dotfiles_dir / module
            linkables.extend(p for p in module_path.rglob('*.symlink')
                             if not p.name.endswith('.symlink.tmpl'))
            templates.extend(module_path.rglob('*.symlink.tmpl'))

        for linkable in linkables:
            module_name = linkable.parts[len(self.dotfiles_dir.parts)]
            rel_path = linkable.relative_to(self.dotfiles_dir / module_name)
            file_path = str(rel_path).replace('.symlink', '')
            target = self.home / f'.{file_path}'

            # Remove symlink
            if target.is_symlink():
                target.unlink()
                print(f"Removed symlink: {target}")

            # Restore backup if it exists
            backup = Path(str(target) + '.backup')
            if backup.exists():
                backup.rename(target)
                print(f"Restored backup: {target}")

        for tmpl in templates:
            module_name = tmpl.parts[len(self.dotfiles_dir.parts)]
            rel_path = tmpl.relative_to(self.dotfiles_dir / module_name)
            file_path = str(rel_path).replace('.symlink.tmpl', '')
            target = self.home / f'.{file_path}'

            # Remove the rendered file (a real file, not a symlink)
            if target.is_file() and not target.is_symlink():
                target.unlink()
                print(f"Removed rendered file: {target}")

            # Restore backup if it exists
            backup = Path(str(target) + '.backup')
            if backup.exists():
                backup.rename(target)
                print(f"Restored backup: {target}")

    def update(self):
        """Update the dotfiles repository from origin."""
        print("Updating dotfiles from origin...")
        subprocess.run(['git', 'fetch', 'origin'], cwd=self.dotfiles_dir, check=False)


def main():
    parser = argparse.ArgumentParser(
        description='Install and manage dotfiles',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=__doc__
    )

    parser.add_argument(
        'command',
        nargs='?',
        default='install',
        choices=['install', 'install_module', 'setup_modules', 'uninstall', 'update'],
        help='Command to run (default: install)'
    )

    parser.add_argument(
        'module',
        nargs='?',
        help='Module name (required for install_module command)'
    )

    parser.add_argument(
        '--symlinks-only',
        action='store_true',
        help='Only create symlinks, skip package installation and scripts'
    )

    args = parser.parse_args()

    installer = DotfilesInstaller()

    try:
        if args.command == 'setup_modules':
            installer.setup_modules()
        elif args.command == 'install':
            installer.install(symlinks_only=args.symlinks_only)
        elif args.command == 'install_module':
            if not args.module:
                print("Error: module name required for install_module command")
                print("Usage: ./install.py install_module <module_name>")
                sys.exit(1)
            print(f"==> Installing dotfiles for module: {args.module}")
            installer.install_module(args.module, symlinks_only=args.symlinks_only)
        elif args.command == 'uninstall':
            installer.uninstall()
        elif args.command == 'update':
            installer.update()
    except KeyboardInterrupt:
        print("\nInstallation cancelled.")
        sys.exit(1)
    except Exception as e:
        print(f"Error: {e}")
        sys.exit(1)


if __name__ == '__main__':
    main()
