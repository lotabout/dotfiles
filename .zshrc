# turn off bell
# set bell-style none

#==============================================================================
# zsh settings
function color_my_prompt {
    local __num_of_jobs="%j"
    local __user_and_host="%F{154}%n@%m%f"
    local __cur_location="%F{012}%1~%f"
    local __git_branch='%F{009}$(git symbolic-ref HEAD 2> /dev/null | cut -d/ -f3 | awk "{print \" (\"\$0\") \"}")%f'
    local __prompt_tail="%F{013}$%f"
    local __last_color="$reset_color"
    export PROMPT="[$__num_of_jobs][$__user_and_host $__cur_location]$__git_branch$__prompt_tail$__last_color "
}

# so that PS1 is replaced
setopt prompt_subst
color_my_prompt

#------------------------------------------------------------
# zsh options

export HISTFILE=~/.zsh_history
export HISTSIZE=50000
export SAVEHIST=50000
setopt inc_append_history

#/v/c/p/p => /var/cache/pacman/pkg
setopt complete_in_word

autoload -U compinit
compinit

# == copied from archwiki ==
# allow approximate
zstyle ':completion:*' completer _complete _match _approximate
zstyle ':completion:*:match:*' original only
zstyle ':completion:*:approximate:*' max-errors 1 numeric

# tab completion for PID :D
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:kill:*' force-list always

# cd not select parent dir
zstyle ':completion:*:cd:*' ignore-parents parent pwd

# useful for path editing Ñ backward-delete-word, but with / as additional delimiter
backward-delete-to-slash () {
  local WORDCHARS=${WORDCHARS//\//}
  zle .backward-delete-word
}
zle -N backward-delete-to-slash

# zsh keybindings
bindkey '\ep' insert-last-word # bind to Alt-p

#==============================================================================
# settings

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

#set -o vi
export EDITOR=vim
export VISUAL=vim

# be confident that the terminal supports 256color
if [[ $TERM == 'xterm' ]]; then
    export TERM='xterm-256color'
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

if [[ "$(uname)" == "Darwin" ]]; then
    # Do something under Mac OS X platform
    alias ls="ls -Gw"
    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
elif [[ "$(expr substr $(uname -s) 1 5)" == "Linux" ]]; then
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

elif [[ "$(expr substr $(uname -s) 1 10)" == "MINGW32_NT" ]]; then
    # Do somthing under MINGW
    :
fi

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

if [ -f ~/.zsh_aliases ]; then
    . ~/.zsh_aliases
fi

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
alias m1='alias g1="cd `pwd`; mdump"'
alias m2='alias g2="cd `pwd`; mdump"'
alias m3='alias g3="cd `pwd`; mdump"'
alias m4='alias g4="cd `pwd`; mdump"'
alias m5='alias g5="cd `pwd`; mdump"'
alias m6='alias g6="cd `pwd`; mdump"'
alias m7='alias g7="cd `pwd`; mdump"'
alias m8='alias g8="cd `pwd`; mdump"'
alias m9='alias g9="cd `pwd`; mdump"'
touch ~/.bookmarks
source ~/.bookmarks

if [[ "$(uname)" == "Darwin" ]]; then
    if [ -f $HOME/.profile ]; then
        source $HOME/.profile
    fi
fi

# install fasd
if hash fasd 2> /dev/null; then
    fasd_cache="$HOME/.fasd-init-bash"
    if [ "$(command -v fasd)" -nt "$fasd_cache" -o ! -s "$fasd_cache" ]; then
        fasd --init posix-alias zsh-hook zsh-ccomp zsh-ccomp-install >| "$fasd_cache"
    fi
    source "$fasd_cache"
    unset fasd_cache
fi

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

if [ -f ~/.zshrc_local ]; then
    . ~/.zshrc_local
fi
