# turn off bell
# set bell-style none
set bell-style visible
# Check for an interactive session
#if [ -z "$PS1" ];then
    #PS1='[\j][\u@\h \W]\$ '
#fi

#==============================================================================
# prompt

function parse_git_dirty() {
    local STATUS=''
    local FLAGS
    FLAGS=('--porcelain' ' --ignore-submodules=dirty' ' --untracked-files=no')
    STATUS=$(command git status ${FLAGS[@]} 2> /dev/null | tail -n1)
    if [[ -n $STATUS ]]; then
        echo " *"
    else
        echo ""
    fi
}

# output the status of the directory
function git_prompt_info() {
    ref=$(command git symbolic-ref HEAD 2> /dev/null) || \
        ref=$(command git rev-parse --short HEAD 2> /dev/null) || return 0
    echo " (${ref#refs/heads/}`parse_git_dirty`)"
}

function color_my_prompt {
    local __num_of_jobs="\j"
    local __user_and_host="\[\033[01;32m\]\u@\h"
    local __cur_location="\[\033[01;34m\]\W\033[0m"
    local __git_branch_color="\[\033[31m\]"
    #local __git_branch='`git branch 2> /dev/null | grep -e ^* | sed -E s/^\\\\\*\ \(.+\)$/\ \(\\\\\1\)\ /`'
    local __git_branch='`git_prompt_info`'
    case $TERM in
        xterm*)
            ;;
        screen*)
            local __prompt_tail="\n\[\033[35m\]→"
            ;;
        *)
            local __prompt_tail="\n\[\033[35m\]$"
        ;;
    esac

    local __last_color="\[\033[00m\]"
    export PS1="[$__num_of_jobs][$__user_and_host $__cur_location]$__git_branch_color$__git_branch$__prompt_tail$__last_color "
}
color_my_prompt

#==============================================================================
# Settings for bash

# enable some bash options
shopt -s histverify
shopt -s histappend
HISTSIZE=100000
HISTFILESIZE=100000
PROMPT_COMMAND='history -a'

#------------------------------------------------------------------------------
# Some other settings

# vi mode
set -o vi

# source profile
if [ -r $HOME/.profile ]; then
    source $HOME/.profile
fi

# be confident that the terminal supports 256color
if [[ $TERM == 'xterm' ]]; then
    export TERM='xterm-256color'
fi

# input method
export XIM=fcitx
export XIM_PROGRAM=/usr/bin/fcitx
export XMODIFIERS="@im=fcitx"
export QT_IM_MODULE=fcitx
export GTK_IM_MODULE=fcitx
DEPENDS="fcitx"

export EDITOR=vim
export VISUAL=vim

if hash nvim 2> /dev/null ;then
    export EDITOR=nvim
    export VISUAL=nvim
fi

PAGE=less

# less command with color
export LESS="-MRg"


if [ "$(uname)" == "Darwin" ]; then
    OS="Mac"
elif [ "$(expr substr $(uname -s) 1 5)" == "Linux" ]; then
    OS="Linux"
elif [ "$(expr substr $(uname -s) 1 10)" == "MINGW32_NT" ]; then
    OS="MinGW"
fi

#==============================================================================
# Completion related


case $OS in
    Mac)
        if [ -f $(brew --prefix)/etc/bash_completion ]; then
            . $(brew --prefix)/etc/bash_completion
        fi

        ;;
    Linux)
        # enhanced bash-completion
        if [ -f /usr/share/bash-completion/bash_completion ]; then
            source /usr/share/bash-completion/bash_completion
        fi
        # load git completion for bash
        if [ -f /usr/doc/git-*/contrib/completion/git-completion.bash ]; then
            source /usr/doc/git-*/contrib/completion/git-completion.bash
        fi

        # ubuntu completion
        if ! shopt -oq posix; then
            if [ -f /usr/share/bash-completion/bash_completion ]; then
                . /usr/share/bash-completion/bash_completion
            elif [ -f /etc/bash_completion ]; then
                . /etc/bash_completion
            fi
        fi

        ;;
    MinGW)
        ;;
esac

# udisk_functions
if [ -f $HOME/bin/udisks_functions ]; then
    source $HOME/bin/udisks_functions
fi

#==============================================================================
# Aliases

if hash gvim 2> /dev/null;then
    alias vim='gvim -v'
    alias vi='gvim -v'
fi

if hash nvim 2> /dev/null ;then
    alias vi='nvim'
fi


case $OS in
    Mac)
        # Do something under Mac OS X platform
        alias ls="ls -Gw"
        alias grep='grep --color=auto'
        alias fgrep='fgrep --color=auto'
        alias egrep='egrep --color=auto'
        ;;
    Linux)
        # Do something under Linux platform
        # enable color support of ls and also add handy aliases
        if [ -x /usr/bin/dircolors ]; then
            test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
            alias ls='ls -B --color=auto'
            alias dir='dir --color=auto'
            alias vdir='vdir --color=auto'

            alias grep='grep --color=auto'
            alias fgrep='fgrep --color=auto'
            alias egrep='egrep --color=auto'
        fi
        ;;
    MinGW)
        ;;
esac

# alias
alias sl='ls'
alias ll='ls -l'
alias l='ls -CF'
#alias grep='grep --color=always'

alias emacs="emacs -nw"
alias ec='emacsclient -t -a ""'

alias cd..="cd .."
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias .....="cd ../../../.."
alias ......="cd ../../../../.."
alias .......="cd ../../../../../.."
alias ........="cd ../../../../../../.."
alias .........="cd ../../../../../../../.."
alias ..........="cd ../../../../../../../../.."

# alias for convenience
alias psg='ps axu | grep'

# alias for racket with readline support
alias racket='LD_PRELOAD=/usr/lib64/libcurses.so racket'

# pppoe shortcuts
alias pt="sudo pppoe-start"
alias pp="sudo pppoe-stop"

# load additional aliases
if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

#==============================================================================
# utilities
function hist() {
    history | awk '{ CMD[$2]++; count++;} END{ for (a in CMD) print CMD[a] " " CMD[a]/count*100 "% " a;}'  \
    | grep -v "./" | column -c3 -s " " -t | sort -nr | nl | head -n10
}

function move_to_trash() {
    if [ ! -d $HOME/.trash ]; then
        mkdir $HOME/.trash
    fi
    for FILE in "$@";
    do
        # remove trailing slash
        local mindtrailingslash=${FILE%/}
        # remove preceding directory path
        local dst=${mindtrailingslash##*/}
        mv -- "$FILE" $HOME/.trash/"${dst}-$(date '+%Y-%m-%d-%T')"
    done

}

function trash_empty() {
    /bin/rm -rI $HOME/.trash && mkdir $HOME/.trash && sync
}

alias rm=move_to_trash
alias trash_empty=trash_empty

# integrate with python virtualenv
function pac() {
    # short for python activate
    localenv=$HOME/localenv

    if [ "$1" ]; then
        localenv=$localenv-$1
    fi

    if [ -f $localenv/bin/activate ]; then
        source $localenv/bin/activate
    fi
}

function pac3() {
    # short for python activate
    localenv=$HOME/localenv-py3
    if [ -f $localenv/bin/activate ]; then
        source $localenv/bin/activate
    fi
}

function pdc() {
    # short for python deactivate
    deactivate
}

# set operation
function set_union () {
   cat $1 $2 | sort | uniq
}

function set_difference () {
   cat $1 $2 $2 | sort | uniq -u
}

# quick bookmark
alias mdump='alias|grep -e "alias g[0-9]"|grep -v "alias m" > ~/.bookmarks'
alias lma='alias | grep -e "alias g[0-9]"|grep -v "alias m"|sed "s/alias //"'
alias m1='alias g1="cd `pwd`"; mdump'
alias m2='alias g2="cd `pwd`"; mdump'
alias m3='alias g3="cd `pwd`"; mdump'
alias m4='alias g4="cd `pwd`"; mdump'
alias m5='alias g5="cd `pwd`"; mdump'
alias m6='alias g6="cd `pwd`"; mdump'
alias m7='alias g7="cd `pwd`"; mdump'
alias m8='alias g8="cd `pwd`"; mdump'
alias m9='alias g9="cd `pwd`"; mdump'
touch ~/.bookmarks
source ~/.bookmarks

# use polipo for proxy
# you'll have to start polipo first
function polipo_shadowsocks(){
    polipo socksParentProxy=localhost:1080
}

function ppp() {
    export http_proxy=http://localhost:8123
    export https_proxy=https://localhost:8123
    export socks_proxy=socks://localhost:8123
    echo "exporting proxy settings for polipo done."
}

#------------------------------------------------------------------------------
# FZF settings

# load fzf if exist
[ -f ~/.fzf.bash ] && source ~/.fzf.bash

# Setting ag as the default source for fzf
export FZF_DEFAULT_COMMAND='(git ls-tree -r --name-only HEAD || ag -l -g "")'
# To apply the command to CTRL-T as well
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"

# integrate with fasd
function j() {
  local dir
  dir="$(fasd -Rdl "$1" | fzf -1 -0 --no-sort +m)" && cd "${dir}" || return 1
}

function v() {
  local file
  file="$(fasd -Rfl "$1" | fzf -1 -0 --no-sort +m)" && vi "${file}" || return 1
}

#------------------------------------------------------------------------------
# fasd settings

# enable fasd
if hash fasd 2> /dev/null; then
    fasd_cache="$HOME/.fasd-init-bash"
    if [ "$(command -v fasd)" -nt "$fasd_cache" -o ! -s "$fasd_cache" ]; then
        fasd --init posix-alias bash-hook bash-ccomp bash-ccomp-install >| "$fasd_cache"
    fi
    source "$fasd_cache"
    unset fasd_cache
fi

#------------------------------------------------------------------------------
# skim settings
export SKIM_DEFAULT_COMMAND='(git ls-tree -r --name-only HEAD || ag -l -g "")'

#------------------------------------------------------------------------------
# used by emacs term mode, change the default-directory.
# https://www.emacswiki.org/emacs/AnsiTermHints#toc5
# It seems that this has some problems, so use zsh instead
function set-eterm-dir {
    echo -e "\033AnSiTu" "$LOGNAME" # $LOGNAME is more portable than using whoami.
    echo -e "\033AnSiTc" "$(pwd)"
    if [ $(uname) = "SunOS" ]; then
        # The -f option does something else on SunOS and is not needed anyway.
        hostname_options="";
    else
        hostname_options="-f";
    fi
    echo -e "\033AnSiTh" "$(hostname $hostname_options)" # Using the -f option can
    # cause problems on some OSes.
    history -a # Write history to disk.
}

# Track directory, username, and cwd for remote logons.
if [ "$TERM" = "eterm-color" ]; then
    PROMPT_COMMAND=set-eterm-dir
fi

#----------  Load other settings --------------
if [ -f ~/.bashrc_local ]; then
    . ~/.bashrc_local
fi

set -o vi
