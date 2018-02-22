from ranger.api.commands import Command
from subprocess import PIPE
from ranger.core.loader import CommandLoader

class sk(Command):
    """
    :sk_select

    Find a file using sk.

    With a prefix argument select only directories.
    """
    def execute(self):
        import subprocess
        import os.path
        if self.quantifier:
            # match only directories
            command="sk --no-multi"
        else:
            # match files and directories
            command="sk --no-multi"
        sk = self.fm.execute_command(command, stdout=subprocess.PIPE)
        stdout, stderr = sk.communicate()
        if sk.returncode == 0:
            sk_file = os.path.abspath(stdout.decode('utf-8').rstrip('\n'))
            if os.path.isdir(sk_file):
                self.fm.cd(sk_file)
            else:
                self.fm.select_file(sk_file)

class fasd(Command):
    """
    use fasd to select the most frequently visited directory
    """
    def execute(self):
        command="fasd -Rdl 2>/dev/null | sk"
        sk = self.fm.execute_command(command, stdout=PIPE)
        stdout, stderr = sk.communicate()
        directory = stdout.decode('utf-8').rstrip('\n')
        self.fm.cd(directory)

class dolphin(Command):
    """open current selected directores/files directly in dolphin"""
    def execute(self):
        import os.path

        marked_files = [f.path for f in self.fm.thisdir.get_selection()]

        if not marked_files:
            return

        if len(marked_files) == 1 and os.path.isdir(marked_files[0]):
            args = ["dolphin"] + marked_files
        else:
            args = ["dolphin", "--select"] + marked_files
        self.fm.loader.add(CommandLoader(args, "open selected files in dolphin"))
