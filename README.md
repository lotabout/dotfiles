This repository keeps the dot files under home folder.

# How to Use This Repository

First clone this repository with:
```sh
git clone --recursive https://github.com/lotabout/dotfiles.git
```

Then follow the instructions described in corresponding sections

## vim && neovim

The configurations in this repository works for both vim and neovim, below is
the recommand settings of using it:
```sh
# 1. copy the files to your home folder
cd <repo directory>
cp .nvimrc .nvim ~
# 2. create symlinks for vim
cd ~
ln -s .nvimrc .vimrc
ln -s .nvim .vim
```
Done!
