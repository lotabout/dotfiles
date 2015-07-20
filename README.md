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

### Plugin Settings
The majority of vim plugins are maintained via Vundle.vim, so you need to
execute `:PluginInstall` inside vim or neovim after the above step.

Note that my airline settings uses unicode characters, and you should install
patched fonts for powerline. You can find it
[here](https://github.com/powerline/fonts).

#### Javascript
I use [vim-autoformat](https://github.com/Chiel92/vim-autoformat) for
formating javascript code, it uses
[js-beautify](https://github.com/einars/js-beautify). You should first install
`node.js` and then execute `npm install -g js-beautify` to install it.

After that, use `:Autoformat` when needed.
