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
texdirs="/opt/texlive/"
for x in $(ls $texdirs 2>/dev/null); do
    dir=`basename "$x"`
    if [[ "$dir" =~ [0-9]+ ]]; then
        # PATH=`find /usr/local/texlive -name i386-linux`:$PATH
        path_prepend "/opt/texlive/$dir/bin/${ARCH}-linux/"
        # MANPATH=`find /usr/local/texlive -type d -name man`:$MANPATH
        MANPATH="/opt/texlive/$dir/texmf-dist/doc/man":$MANPATH
        # INFOPATH=`find /usr/local/texlive -type d -name info`:$INFOPATH
        INFOPATH="/opt/texlive/$dir/texmf-dist/doc/info":$INFOPATH
    fi
done

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Ruby
if hash ruby &> /dev/null && hash gem &> /dev/null; then
    path_prepend "$(ruby -rubygems -e 'puts Gem.user_dir')/bin"
fi

if [ -d $HOME/.rvm/bin ]; then
    path_prepend $HOME/.rvm/bin
    [[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*
fi

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Rust
if [ -d $HOME/.cargo/bin ]; then
    path_prepend $HOME/.cargo/bin
fi

# rustup
export RUSTUP_DIST_SERVER=https://mirrors.ustc.edu.cn/rust-static
export RUSTUP_UPDATE_ROOT=https://mirrors.ustc.edu.cn/rust-static/rustup

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Anaconda
if [ -d $HOME/anaconda3 ]; then
    path_prepend $HOME/anaconda3/bin
fi

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Go
export GOPATH=${GOPATH:-$HOME/go}
if [ -d $GOPATH/bin ]; then
    path_append $GOPATH/bin;
fi

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Hahoop
alias hstart="/usr/local/Cellar/hadoop/3.0.0/sbin/start-dfs.sh;/usr/local/Cellar/hadoop/3.0.0/sbin/start-yarn.sh"
alias hstop="/usr/local/Cellar/hadoop/3.0.0/sbin/stop-yarn.sh;/usr/local/Cellar/hadoop/3.0.0/sbin/stop-dfs.sh"

#==============================================================================

export PATH
export MANPATH
export INFOPATH

case $OS in
    Mac)
        export LC_ALL="en_US.UTF-8"
        export LANG="en_US.UTF-8"

        export HOMEBREW_BOTTLE_DOMAIN=https://mirrors.ustc.edu.cn/homebrew-bottles
        ;;
    *)
        export LC_CTYPE="zh_CN.utf8"
        export LANG=en_US.utf8
        ;;
esac
