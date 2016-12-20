#!/bin/bash

#==============================================================================
if [[ "$(uname)" == "Darwin" ]]; then
    OS="Mac"
elif [ "$(expr substr $(uname -s) 1 5)" == "Linux" ]; then
    OS="Linux"
elif [ "$(expr substr $(uname -s) 1 10)" == "MINGW32_NT" ]; then
    OS="MinGW"
fi

#==============================================================================
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

BACKUP_FILES=(".vim" ".vimrc" ".config/nvim" ".bash_profile" ".bashrc" ".zshrc" ".tmux.conf" ".ideavimrc" ".Xresources" "bin")

# remove old links or backup old files/dirs
for FILE in "${BACKUP_FILES[@]}"
do
    if [[ -L $HOME/$FILE ]]; then
        rm $HOME/$FILE
    elif [[ -r $HOME/$FILE ]]; then
        mv $HOME/$FILE $HOME/$FILE.bak-$(date +"%F-%R")
    fi
done


AUTO_LINK_FILES=(".config/nvim" ".bash_profile" ".zshrc" ".tmux.conf" ".ideavimrc" ".Xresources" "bin")

# add symlinks
for FILE in "${AUTO_LINK_FILES[@]}"
do
    ln -s $DIR/$FILE $HOME/$FILE
done

# Link indivisual ones
ln -s $DIR/.bash_profile $HOME/.bashrc
ln -s $DIR/.config/nvim/init.vim $HOME/.vimrc
ln -s $DIR/.config/nvim $HOME/.vim

#==============================================================================
# zsh plugins

mkdir -p $HOME/.zsh
if [[ ! -d ~/.zsh/zsh-autosuggestions ]];then
    git clone git://github.com/zsh-users/zsh-autosuggestions ~/.zsh/zsh-autosuggestions
fi

#==============================================================================
# install dependencies

#==============================================================================
# Post install

# install neovim python support
if hash pip2 > /dev/null; then
    pip2 install --user --upgrade neovim
fi

if hash pip3 > /dev/null; then
    pip3 install --user --upgrade neovim
fi

# install vim plugins
vi -c ":PlugInstall | qa"
