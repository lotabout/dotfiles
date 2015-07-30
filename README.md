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

Some plugins require python support, vim normally has it already, but neovim
has not. Please refer to
[nvim_python](http://neovim.io/doc/user/nvim_python.html) for how to install
it.

### Plugin Settings
The majority of vim plugins are maintained via Vundle.vim, so you need to
execute `:PluginInstall` inside vim or neovim after the above step.

#### airline
Note that my airline settings uses unicode characters, and you should install
patched fonts for powerline. You can find it
[here](https://github.com/powerline/fonts).

#### ctrlp-cmatcher
[ctrlp-cmatcher](https://github.com/JazzCore/ctrlp-cmatcher) adapt CommandT
version of matching algorithm.

It requires additional settings:
```
cd ~/.vim/bundle/ctrlp-cmatcher
./install.sh
```

#### Javascript
I use [vim-autoformat](https://github.com/Chiel92/vim-autoformat) for
formating javascript code, it uses
[js-beautify](https://github.com/einars/js-beautify). You should first install
`node.js` and then execute `npm install -g js-beautify` to install it.

After that, use `:Autoformat` when needed.

