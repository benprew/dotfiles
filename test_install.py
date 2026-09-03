import subprocess
import unittest
from pathlib import Path
from unittest import mock

import install


class DotfilesInstallerTest(unittest.TestCase):
    def setUp(self):
        self.installer = install.DotfilesInstaller()
        self.installer.dotfiles_dir = Path(__file__).parent.resolve()

    def test_install_aggregates_packages_and_grammars(self):
        modules = ['core', 'ruby', 'shell']

        with (mock.patch.object(self.installer, 'setup_modules'),
              mock.patch.object(self.installer, 'get_modules',
                                return_value=modules),
              mock.patch.object(self.installer, 'install_module_files') as files,
              mock.patch.object(self.installer, 'install_packages') as packages,
              mock.patch.object(self.installer, 'install_module_scripts') as scripts,
              mock.patch.object(
                  self.installer, 'install_tree_sitter_grammars') as grammars):
            self.installer.install()

        self.assertEqual(
            [mock.call(module) for module in modules], files.call_args_list)
        packages.assert_called_once_with(modules)
        self.assertEqual(
            [mock.call(module) for module in modules], scripts.call_args_list)
        grammars.assert_called_once_with(modules)

    def test_linux_packages_are_deduplicated_in_one_command(self):
        completed = subprocess.CompletedProcess([], 0)

        def executable(name):
            return '/usr/bin/apt' if name == 'apt' else None

        with (mock.patch.object(install.sys, 'platform', 'linux'),
              mock.patch.object(install.shutil, 'which', side_effect=executable),
              mock.patch.object(
                  install.subprocess, 'run', return_value=completed) as run):
            self.installer.install_packages(['core', 'pop_os', 'finance'])

        run.assert_called_once()
        command = run.call_args.args[0]
        self.assertEqual(['sudo', 'apt', 'install', '-y'], command[:4])
        self.assertEqual(1, command.count('lm-sensors'))
        self.assertIn('ledger', command)

    def test_brewfiles_are_combined_in_one_bundle(self):
        completed = subprocess.CompletedProcess([], 0)
        aggregate_contents = []

        def run_bundle(command, **_kwargs):
            aggregate_path = Path(command[2].removeprefix('--file='))
            aggregate_contents.append(aggregate_path.read_text())
            return completed

        with (mock.patch.object(install.sys, 'platform', 'darwin'),
              mock.patch.object(
                  install.subprocess, 'run', side_effect=run_bundle) as run):
            self.installer.install_packages(['core', 'finance', 'git'])

        run.assert_called_once()
        self.assertIn("brew 'ripgrep'", aggregate_contents[0])
        self.assertIn("brew 'ledger'", aggregate_contents[0])
        self.assertIn("brew 'git-delta'", aggregate_contents[0])

    def test_tree_sitter_grammars_use_one_emacs_process(self):
        completed = subprocess.CompletedProcess([], 0)

        with (mock.patch.object(
                  install.shutil, 'which', return_value='/usr/bin/emacs'),
              mock.patch.object(
                  install.subprocess, 'run', return_value=completed) as run):
            self.installer.install_tree_sitter_grammars(['core', 'ruby'])

        run.assert_called_once()
        command = run.call_args.args[0]
        self.assertEqual(['/usr/bin/emacs', '--batch', '--quick', '--eval'],
                         command[:4])
        expression = command[4]
        self.assertIn('(json "https://github.com/tree-sitter/tree-sitter-json")',
                      expression)
        self.assertIn('(ruby "https://github.com/tree-sitter/tree-sitter-ruby")',
                      expression)

    def test_c_module_tree_sitter_grammar_and_packages(self):
        grammars = self.installer.get_tree_sitter_grammars(['c'])
        self.assertIn('c', grammars)
        self.assertIn('cpp', grammars)
        self.assertEqual(
            ('https://github.com/tree-sitter/tree-sitter-c',), grammars['c']
        )
        self.assertEqual(
            ('https://github.com/tree-sitter/tree-sitter-cpp',), grammars['cpp']
        )


if __name__ == '__main__':
    unittest.main()
