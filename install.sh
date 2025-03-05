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

BACKUP_FILES=(".vim" ".vimrc" ".config/nvim" ".profile" ".bash_profile" ".bashrc" ".zshrc" ".tmux.conf" ".ideavimrc" ".Xresources" "bin" ".config/ranger" ".tigrc" ".alacritty.yml" ".config/kitty")

# remove old links or backup old files/dirs
for FILE in "${BACKUP_FILES[@]}"
do
    if [[ -L $HOME/$FILE ]]; then
        rm $HOME/$FILE
    elif [[ -r $HOME/$FILE ]]; then
        mv $HOME/$FILE $HOME/$FILE.bak-$(date +"%F-%R")
    fi
done


AUTO_LINK_FILES=(".config/nvim" ".profile" ".bash_profile" ".zshrc" ".tmux.conf" ".ideavimrc" ".Xresources" "bin" ".config/ranger" ".tigrc" ".alacritty.yml" ".config/kitty")

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

# auto suggestion
mkdir -p $HOME/.zsh
if [[ ! -d ~/.zsh/zsh-autosuggestions ]];then
    git clone --depth=1 https://github.com/zsh-users/zsh-autosuggestions.git ~/.zsh/zsh-autosuggestions
fi

#==============================================================================
# install dependencies

# fasd
if [[ ! -f $DIR/bin/fasd ]]; then
    curl https://raw.githubusercontent.com/clvv/fasd/master/fasd -o $DIR/bin/fasd
    chmod +x $DIR/bin/fasd
fi

case $OS in
    Mac)
        if hash diff-so-fancy &> /dev/null; then
            brew install diff-so-fancy
        fi
        ;;
    Linux)
        ;;
esac

git config --global core.pager "diff-so-fancy | less --tabs=4 -RF"
git config --global interactive.diffFilter "diff-so-fancy --patch"

#==============================================================================
# Post install

# install neovim python support
if hash pip2 &> /dev/null; then
    pip2 install --user --upgrade neovim
    pip2 install --user psutil setproctitle # for ncm
fi

if hash pip3 &> /dev/null; then
    pip3 install --user --upgrade neovim
    pip3 install --user psutil setproctitle # for ncm
fi

# install LSP server for rust, python
if hash rustup &> /dev/null; then
    rustup component add rls-preview rust-analysis rust-src
fi

if hash pip2 &> /dev/null; then
    pip2 install python-language-server
fi

if hash pip3 &> /dev/null; then
    pip3 install python-language-server
fi

# install vim plugins
vi -c ":PlugInstall | qa"
