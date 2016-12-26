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

path_remove ()  { export PATH=`echo -n $PATH | awk -v RS=: -v ORS=: '$0 != "'$1'"' | sed 's/:$//'`; }
path_append ()  { path_remove $1; export PATH="$PATH:$1"; }
path_prepend () { path_remove $1; export PATH="$1:$PATH"; }

#==============================================================================
# Path settings

# local binary path
PATH=$HOME/bin:/sbin:/usr/sbin:$PATH:/usr/games/bin

# texlive
texdirs="/opt/texlive/*"
for x in $texdirs; do
    dir=`basename "$x"`
    if [[ "$dir" =~ [0-9]+ ]]; then
        # PATH=`find /usr/local/texlive -name i386-linux`:$PATH
        PATH="/opt/texlive/$dir/bin/${ARCH}-linux/":$PATH
        # MANPATH=`find /usr/local/texlive -type d -name man`:$MANPATH
        MANPATH="/opt/texlive/$dir/texmf-dist/doc/man":$MANPATH
        # INFOPATH=`find /usr/local/texlive -type d -name info`:$INFOPATH
        INFOPATH="/opt/texlive/$dir/texmf-dist/doc/info":$INFOPATH
    fi
done

# add path for ruby gems
if which ruby >/dev/null && which gem >/dev/null; then
    PATH="$(ruby -rubygems -e 'puts Gem.user_dir')/bin:$PATH"
fi

if [ -d $HOME/.rvm/bin ]; then
    export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting
    [[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*
fi

if [ -d $HOME/.cargo/bin ]; then
    PATH="$HOME/.cargo/bin:$PATH"
fi

#==============================================================================

export PATH
export MANPATH
export INFOPATH

case $OS in
    Mac)
        export LC_ALL="en_US.UTF-8"
        export LANG="en_US.UTF-8"
        ;;
    *)
        export LC_CTYPE="zh_CN.utf8"
        export LANG=en_US.utf8
        ;;
esac
