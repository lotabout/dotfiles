# turn off bell
# set bell-style none
set bell-style visible
# Check for an interactive session
#if [ -z "$PS1" ];then
    #PS1='[\j][\u@\h \W]\$ '
#fi
function color_my_prompt {
    local __num_of_jobs="\j"
    local __user_and_host="\[\033[01;32m\]\u@\h"
    local __cur_location="\[\033[01;34m\]\W"
    local __git_branch_color="\[\033[31m\]"
    #local __git_branch="\`ruby -e \"print (%x{git branch 2> /dev/null}.grep(/^\*/).first || '').gsub(/^\* (.+)$/, '(\1) ')\"\`"
    local __git_branch='`git branch 2> /dev/null | grep -e ^* | sed -E s/^\\\\\*\ \(.+\)$/\ \(\\\\\1\)\ /`'
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

## source profile
if [ -r $HOME/.profile ]; then
    source $HOME/.profile
fi

# input method
export XIM=fcitx
export XIM_PROGRAM=/usr/bin/fcitx
export XMODIFIERS="@im=fcitx"
export QT_IM_MODULE=fcitx
export GTK_IM_MODULE=fcitx
DEPENDS="fcitx"
#export GTK_IM_MODULE=xim
# ibus setting
# export GTK_IM_MODULE=ibus
# export XMODIFIERS=@im=ibus
# export QT_IM_MODULE=ibus
# export GTK_IM_MODULE="scim-bridge"
# export XMODIFIERS="@im=SCIM"
# export QT_IM_MODULE="scim-bridge"
# export XIM_PROGRAM="/usr/bin/scim -d"
# export XIM="scim-bridge"
# export XMODIFIERS=@im=yong
# export GTK_IM_MODULE=yong
# export QT_IM_MODULE=xim

#set -o vi
export EDITOR=vim
export VISUAL=vim

# be confident that the terminal supports 256color
if [[ $TERM == 'xterm' ]]; then
    export TERM='xterm-256color'
fi

# set to correct term for TMUX according to termcap info
#if [[ $TMUX ]]; then
    #if [[ $TERM =~ 256color ]]; then
        #if infocmp xterm-screen-256color &>/dev/null; then
            #export TERM='xterm-screen-256color'
        #fi
    #fi
#fi

# enhanced bash-completion
if [ -f /usr/share/bash-completion/bash_completion ]; then
    source /usr/share/bash-completion/bash_completion
    # bind 'TAB:menu-complete'
    # bind "set show-all-if-ambiguous on"
fi

# load git completion for bash
if [ -f /usr/doc/git-*/contrib/completion/git-completion.bash ]; then
    source /usr/doc/git-*/contrib/completion/git-completion.bash
fi

# load completion for homebrew installed software
if [ -d /usr/local/etc/bash_completion.d ]; then
    source /usr/local/etc/bash_completion.d/git-completion.bash &> /dev/null
    source /usr/local/etc/bash_completion.d/ag.bashcomp.sh &> /dev/null
    source /usr/local/etc/bash_completion.d/npm &> /dev/null
fi

# ubuntu completion
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

#source /usr/doc/tmux-1.5/examples/bash_completion_tmux.sh

# enable some bash options
shopt -s histverify
shopt -s histappend
HISTSIZE=10000
HISTFILESIZE=10000
PROMPT_COMMAND='history -a'

# udisk_functions
if [ -f $HOME/bin/udisks_functions ]; then
    source $HOME/bin/udisks_functions
fi

# less command with color
export LESS="-MRg"

if hash gvim 2> /dev/null;then
    alias vim='gvim -v'
    alias vi='gvim -v'
fi

if hash nvim 2> /dev/null ;then
    alias vi='nvim'
fi

if [ "$(uname)" == "Darwin" ]; then
    # Do something under Mac OS X platform
    alias ls="ls -Gw"
    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
elif [ "$(expr substr $(uname -s) 1 5)" == "Linux" ]; then
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

elif [ "$(expr substr $(uname -s) 1 10)" == "MINGW32_NT" ]; then
    # Do somthing under MINGW
    :
fi



# alias
alias sl='ls'
alias ll='ls -l'
alias l='ls -CF'
#alias grep='grep --color=always'

alias emacs="emacs -nw"
alias ec='emacsclient -t -a ""'

# turn off touchpad
# synclient touchpadoff=1
PAGE=less

# alias for convenience
alias psg='ps axu | grep'

# alias for racket with readline support
alias racket='LD_PRELOAD=/usr/lib64/libcurses.so racket'

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# utilities
function hist() {
    history | awk '{ CMD[$2]++; count++;} END{ for (a in CMD) print CMD[a] " " CMD[a]/count*100 "% " a;}'  \
    | grep -v "./" | column -c3 -s " " -t | sort -nr | nl | head -n10
}

function move_to_trash() {
    if [ ! -d $HOME/.trash ]; then
        mkdir $HOME/.trash
    fi
    mv "$@" $HOME/.trash
}
function trash_empty() {
    /bin/rm -rI $HOME/.trash && mkdir $HOME/.trash && sync
}

alias rm=move_to_trash
alias trash_empty=trash_empty

# pppoe shortcuts
alias pt="sudo pppoe-start"
alias pp="sudo pppoe-stop"

# integrate with python virtualenv
function pac() {
    # short for python activate
    localenv=$HOME/localenv
    if [ -f $localenv/bin/activate ]; then
        source $localenv/bin/activate
    fi
}

function pdc() {
    # short for python deactivate
    deactivate
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
function ppp() {
    export http_proxy=http://localhost:8123
    export https_proxy=https://localhost:8123
    export socks_proxy=socks://localhost:8123
    echo "exporting proxy settings for polipo done."
}

function polipo_shadowsocks(){
    polipo socksParentProxy=localhost:1080
}

#----------  FZF settings --------------
# Setting ag as the default source for fzf
export FZF_DEFAULT_COMMAND='(git ls-tree -r --name-only HEAD || ag -l -g "")'
# To apply the command to CTRL-T as well
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"

# integrate with fasd
j() {
  local dir
  dir="$(fasd -Rdl "$1" | fzf -1 -0 --no-sort +m)" && cd "${dir}" || return 1
}
v() {
  local file
  file="$(fasd -Rfl "$1" | fzf -1 -0 --no-sort +m)" && vi "${file}" || return 1
}

#----------  Load other settings --------------

if [ "$(uname)" == "Darwin" ]; then
    if [ -f $HOME/.profile ]; then
        source $HOME/.profile
    fi
fi

if [ -f ~/.bashrc_local ]; then
    . ~/.bashrc_local
fi
set -o vi
[ -f ~/.fzf.bash ] && source ~/.fzf.bash
