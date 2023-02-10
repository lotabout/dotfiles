#!/bin/bash

# set architecture
if [ -z "$ARCH" ]; then
  case "$( uname -m )" in
    i?86) ARCH=i486 ;;
    arm*) ARCH=arm ;;
       *) ARCH=$( uname -m ) ;;
  esac
fi

# set operating system
if [[ "$(uname)" == "Darwin" ]]; then
    OS="Mac"
elif [[ "$(expr substr $(uname -s) 1 5)" == "Linux" ]]; then
    OS="Linux"
elif [[ "$(expr substr $(uname -s) 1 10)" == "MINGW32_NT" ]]; then
    OS="MinGW"
fi

#==============================================================================
# Bash toolbox.

function path_remove {
  # ref: https://unix.stackexchange.com/a/291611
  # Delete path by parts so we can never accidentally remove sub paths
  if [ "$PATH" == "$1" ] ; then PATH="" ; fi
  PATH=${PATH//":$1:"/":"} # delete any instances in the middle
  PATH=${PATH/#"$1:"/} # delete any instance at the beginning
  PATH=${PATH/%":$1"/} # delete any instance in the at the end
}
path_append ()  { path_remove $1; export PATH="$PATH:$1"; }
path_prepend () { export PATH="$1:$PATH"; }

#==============================================================================
# Path settings

# local binary path
PATH=$HOME/bin:/sbin:/usr/sbin:$PATH:/usr/games/bin

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# npm
if [ -d $HOME/node ]; then
    path_prepend $HOME/node
fi

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Ruby
#if hash ruby &> /dev/null && hash gem &> /dev/null; then
#    path_prepend "$(ruby -rubygems -e 'puts Gem.user_dir')/bin"
#fi
#
#if [ -d $HOME/.rvm/bin ]; then
#    path_prepend $HOME/.rvm/bin
#    [[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*
#fi

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Rust

# rustup
export RUSTUP_DIST_SERVER=https://mirrors.ustc.edu.cn/rust-static
export RUSTUP_UPDATE_ROOT=https://mirrors.ustc.edu.cn/rust-static/rustup

if [ -d $HOME/.cargo/bin ]; then
    path_prepend $HOME/.cargo/bin
fi


if [ -f "$HOME/.cargo/env" ]; then
    source "$HOME/.cargo/env" 
fi

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Anaconda
if [ -d $HOME/anaconda3 ]; then
    path_prepend $HOME/anaconda3/bin
fi

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Go
export GOPATH=${GOPATH:-$HOME/go}
if [ -d $GOPATH/bin ]; then
    path_prepend $GOPATH/bin;
fi

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# spark
PYTHONPATH=$SPARK_HOME/python:$SPARK_HOME/python/build:$PYTHONPATH
if hash ipython 2> /dev/null; then
    export PYSPARK_DRIVER_PYTHON=ipython
fi

#==============================================================================

case $OS in
    Mac)
        export LC_ALL="en_US.UTF-8"
        export LANG="en_US.UTF-8"

        export HOMEBREW_BOTTLE_DOMAIN=https://mirrors.ustc.edu.cn/homebrew-bottles
        export COPYFILE_DISABLE=true
        ;;
    Linux)
        export LC_ALL="en_US.UTF-8"
        ;;
    *)
        export LC_CTYPE="zh_CN.utf8"
        export LANG=en_US.utf8
        ;;
esac

#==============================================================================
# load local settings
if [[ -f ~/.profile-local ]]; then
    . ~/.profile-local
fi
