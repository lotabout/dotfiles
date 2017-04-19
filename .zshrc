# turn off bell
# set bell-style none

#==============================================================================
# zsh settings

# Checks if working tree is dirty
function parse_git_dirty() {
    local STATUS=''
    local FLAGS
    FLAGS=('--porcelain' '--ignore-submodules=dirty' '--untracked-files=no')
    STATUS=$(command git status ${FLAGS} 2> /dev/null | tail -n1)
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
    echo " (${ref#refs/heads/}$(parse_git_dirty))"
}

function color_my_prompt {
    local __num_of_jobs="%j"
    local __user_and_host="%F{154}%n@%m%f"
    local __cur_location="%F{012}%1~%f"
    local __git_branch='%F{009}$(git_prompt_info)%f'
    local __prompt_tail="%F{013}$%f"
    local __last_color="$reset_color"
    export PROMPT="[$__num_of_jobs][$__user_and_host $__cur_location]$__git_branch"$'\n'"$__prompt_tail$__last_color "
}

# so that PS1 is replaced
setopt prompt_subst
color_my_prompt

#============================================================
# zsh options

export HISTFILE=~/.zsh_history
export HISTSIZE=50000
export SAVEHIST=50000
setopt inc_append_history

#/v/c/p/p => /var/cache/pacman/pkg
setopt complete_in_word

autoload -U compinit
compinit

# enable extended glob
setopt extendedglob

#------------------------------------------------------------
# Completion

# Use cache
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.zsh/cache

# allow approximate
zstyle ':completion:*' completer _complete _match _approximate
zstyle ':completion:*:match:*' original only
zstyle ':completion:*:approximate:*' max-errors 1 numeric

# tab completion for PID :D
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:kill:*' force-list always

# cd not select parent dir
zstyle ':completion:*:cd:*' ignore-parents parent pwd

# useful for path editing, backward-delete-word, but with / as additional delimiter
backward-delete-to-slash () {
local WORDCHARS=${WORDCHARS//\//}
zle .backward-delete-word
}
zle -N backward-delete-to-slash

# zsh keybindings
bindkey '\ep' insert-last-word # bind to Alt-p

#==============================================================================
# settings

# vi mode
set -o vi

## source profile
[[ -r $HOME/.profile ]] && source $HOME/.profile

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

if hash nvim 2> /dev/null; then
    export EDITOR=nvim
    export VISUAL=nvim
fi

# less command with color
export LESS="-MRg"

if [[ "$(uname)" == "Darwin" ]]; then
    OS="Mac"
elif [[ "$(expr substr $(uname -s) 1 5)" == "Linux" ]]; then
    OS="Linux"
elif [[ "$(expr substr $(uname -s) 1 10)" == "MINGW32_NT" ]]; then
    OS="MinGW"
fi

#==============================================================================

#==============================================================================
# Aliases

if hash gvim 2> /dev/null;then
    alias vim='gvim -v'
fi

# prefer neovim over vim
if hash nvim 2> /dev/null ;then
    alias vi='nvim'
fi

case $OS in
    Mac)
        alias ls="ls -Gw"
        alias grep='grep --color=auto'
        alias fgrep='fgrep --color=auto'
        alias egrep='egrep --color=auto'
        ;;
    Linux)
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

alias sl='ls'
alias ll='ls -lh'
alias l='ls -CF'
#alias grep='grep --color=always'

alias emacs="emacs -nw"
alias ec='emacsclient -t -a ""'

# alias for convenience
alias psg='ps axu | grep'
alias cd..="cd .."
alias -g ..=".."
alias -g ...="../.."
alias -g ....="../../.."
alias -g .....="../../../.."
alias -g ......="../../../../.."
alias -g .......="../../../../../.."
alias -g ........="../../../../../../.."
alias -g .........="../../../../../../../.."
alias -g ..........="../../../../../../../../.."

if [ -f ~/.zsh_aliases ]; then
    . ~/.zsh_aliases
fi

#==============================================================================
# utilities
#
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

alias rm=move_to_trash
alias trash_empty=trash_empty

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

# ftpane - switch pane (@george-b)
function ftpane() {
  local panes current_window current_pane target target_window target_pane
  panes=$(tmux list-panes -s -F '#I:#P - #{pane_current_path} #{pane_current_command}')
  current_pane=$(tmux display-message -p '#I:#P')
  current_window=$(tmux display-message -p '#I')

  target=$(echo "$panes" | grep -v "$current_pane" | sk -m) || return

  target_window=$(echo $target | awk 'BEGIN{FS=":|-"} {print$1}')
  target_pane=$(echo $target | awk 'BEGIN{FS=":|-"} {print$2}' | cut -c 1)

  if [[ $current_window -eq $target_window ]]; then
    tmux select-pane -t ${target_window}.${target_pane}
  else
    tmux select-pane -t ${target_window}.${target_pane} &&
    tmux select-window -t $target_window
  fi
}

#------------------------------------------------------------------------------
# SKIM settings

# load fzf if exist
[[ -f ~/.skim.zsh ]] && source ~/.skim.zsh

# Setting ag as the default source for skim
export SKIM_DEFAULT_COMMAND='(git ls-tree -r --name-only HEAD || ag -l -g "")'
# To apply the command to CTRL-T as well
export SKIM_CTRL_T_COMMAND="$SKIM_DEFAULT_COMMAND"

# integrate with fasd
function j() {
  local dir
  dir="$(fasd -Rdl "$1" | sk +m)" && cd "${dir}" || return 1
}

function v() {
  local file
  file="$(fasd -Rfl "$1" | sk +m)" && vi "${file}" || return 1
}

#------------------------------------------------------------------------------
# fasd settings

# enable fasd
if hash fasd 2> /dev/null; then
    fasd_cache="$HOME/.fasd-init-zsh"
    if [ "$(command -v fasd)" -nt "$fasd_cache" -o ! -s "$fasd_cache" ]; then
        fasd --init posix-alias zsh-hook >| "$fasd_cache"
    fi

    source "$fasd_cache"
    unset fasd_cache
fi

#------------------------------------------------------------------------------
# used by emacs term mode, change the default-directory.
# https://www.emacswiki.org/emacs/AnsiTermHints#toc5
function eterm-update {
    # Only set the full hostname for ssh sessions.
    if [[ -n "$SSH_CONNECTION" ]]; then
        # For ssh connections, use the hostname (it is assumed here
        # that there is an ssh alias on the connecting machine that has
        # the same name as the ssh alias).
        echo -e "\033AnSiTh" "$(hostname)"
    else
        # For local sessions, use the full host name so tramp will know
        # that the path is not remote.
        echo -e "\033AnSiTh" "$(hostname -f)"
    fi

    echo -e "\033AnSiTu" "$LOGNAME" # $LOGNAME is more portable than using whoami.
    echo -e "\033AnSiTc" "$(pwd)"
}

function chpwd {
    eterm-update
}

#==============================================================================
# plugins
AUTO_SUGGESTIONS=$HOME/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh
if [[ -r $AUTO_SUGGESTIONS && $TERM =~ ".*256" ]]; then
    source $AUTO_SUGGESTIONS
    ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=030'
fi

#==============================================================================
# load other settings
if [[ -f ~/.zshrc-local ]]; then
    . ~/.zshrc-local
fi
