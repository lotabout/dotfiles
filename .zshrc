#zmodload zsh/zprof # top of your .zshrc file
#==============================================================================
# turn off bell
# set bell-style none

[ $TERM = "dumb" ] && PS1='$ ' && return

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
    local __user_and_host="%F{blue}%n@%m%f"
    local __cur_location="%F{012}%1~%f"
    local __git_branch='%F{009}$(git_prompt_info)%f'
    local __prompt_tail="%F{013}$%f"
    local __last_color="$reset_color"
    local __current_time='%F{002}$(date +%H:%M:%S)%f'
    export PROMPT="[$__num_of_jobs][$__current_time][$__user_and_host $__cur_location]$__git_branch"$'\n'"$__prompt_tail$__last_color "
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
setopt hist_ignore_dups

#/v/c/p/p => /var/cache/pacman/pkg
setopt complete_in_word

# only initialize once a day
autoload -Uz compinit
if [ $(date +'%j') != $(/usr/bin/stat -f '%Sm' -t '%j' ${ZDOTDIR:-$HOME}/.zcompdump) ]; then
  compinit
else
  compinit -C
fi

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

# highlight the selections of completion
zstyle ':completion:*' menu select

# useful for path editing, backward-delete-word, but with / as additional delimiter
backward-delete-to-slash () {
local WORDCHARS=${WORDCHARS//\//}
zle .backward-delete-word
}
zle -N backward-delete-to-slash

# zsh keybindings
bindkey -e # emacs mode

bindkey '\ep' insert-last-word # bind to Alt-p
bindkey '^[' vi-cmd-mode
export KEYTIMEOUT=1

#==============================================================================
# settings

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

if command -v nvim &> /dev/null; then
    export EDITOR=nvim
    export VISUAL=nvim
fi

# less command with color
export LESS="-MRg"

#==============================================================================
# Aliases

if command -v gvim &> /dev/null;then
    alias vim='gvim -v'
fi

# prefer neovim over vim
if command -v nvim &> /dev/null ;then
    alias vi='nvim'
fi

case $OS in
    Mac)
        alias ls="ls -Gw"
        alias grep='grep --color=auto'
        alias fgrep='fgrep --color=auto'
        alias egrep='egrep --color=auto'
        alias ctags="`brew --prefix`/bin/ctags"
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

alias emacs="emacs -nw"
alias ec='emacsclient -t -a ""'

if command -v exa &> /dev/null; then
    alias ls="exa"
fi

# alias for convenience
alias psg='ps axu | grep'
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

if [ -f ~/.zsh_aliases ]; then
    . ~/.zsh_aliases
fi

alias scpr="rsync -P --rsh=ssh"

alias cnpm="npm --registry=https://registry.npm.taobao.org \
    --cache=$HOME/.npm/.cache/cnpm \
    --disturl=https://npm.taobao.org/dist \
    --userconfig=$HOME/.cnpmrc"

alias occ='gcc -framework Foundation'

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

function trash_empty() {
    mv $HOME/.trash $HOME/.trash.to-remove
    mkdir $HOME/.trash
    /bin/rm -rf $HOME/.trash.to-remove
}

alias rm=move_to_trash
alias trash_empty=trash_empty

function pac() {
    # short for python activate
    localenv=$HOME/localenv

    if [ "$1" ]; then
        localenv=$localenv-$1
    fi

    if [ ! -d $localenv ]; then
        localenv=$HOME/anaconda3
    fi

    if [ -f $localenv/bin/activate ]; then
        source $localenv/bin/activate $1
    fi
}

function pac3() {
    pac py3
}

function pdc() {
    # short for python deactivate
    deactivate &> /dev/null || source deactivate
}


# set operation
function set_union () {
   cat $1 $2 | sort | uniq
}

function set_difference () {
   cat $1 $2 $2 | sort | uniq -u
}

function set_intersection() {
    comm -12 <(sort $1) <(sort $2)
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

case $OS in
    Mac)
        function ppp() {
            export http_proxy=http://localhost:1087
            export https_proxy=http://localhost:1087
            export socks_proxy=socks://localhost:8123
            echo 'proxy = http://localhost:1087' > ~/.curlrc
            echo "exporting proxy settings for SS done."
        }
        ;;
    Linux)
        function ppp() {
            export http_proxy=http://localhost:8123
            export https_proxy=https://localhost:8123
            export socks_proxy=socks://localhost:8123
            echo 'proxy = http://localhost:8123' > ~/.curlrc
            echo "exporting proxy settings for polipo done."
        }
        ;;
esac

function upp() {
    unset http_proxy
    unset https_proxy
    unset socks_proxy
    echo '' > ~/.curlrc
    echo "Unset proxy settingsdone."
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

# load if exist
if [ -d $HOME/.skim ]; then
    if [[ ! "$PATH" == *$HOME/.skim/bin* ]]; then
      export PATH="$PATH:$HOME/.skim/bin"
    fi

    # Setting ag as the default source for skim
    export SKIM_DEFAULT_COMMAND='fd --type f || git ls-files -c -o --exclude-standard || rg -l "" || ag -l -g "" || find . -type f'
    # To apply the command to CTRL-T as well
    export SKIM_CTRL_T_COMMAND="$SKIM_DEFAULT_COMMAND"

    # integrate with fasd
    function j() {
      local dir
      dir="$(fasd -Rdl "$1" | sk -m)" && cd "${dir}" || return 1
    }

    function v() {
      local file
      file="$(fasd -Rfl "$1" | sk -m)" && vi "${file}" || return 1
    }


    # Auto-completion
    # ---------------
    [[ $- == *i* ]] && source "$HOME/.skim/shell/completion.zsh" 2> /dev/null

    # Key bindings
    # ------------
    source "$HOME/.skim/shell/key-bindings.zsh"


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # intergration with git
    # modified from https://gist.github.com/junegunn/8b572b8d4b5eddd8b85e5f4d40f17236
    is_in_git_repo() {
      git rev-parse HEAD > /dev/null 2>&1
    }

    fzf-down() {
      sk --bind 'alt-a:select-all,alt-d:deselect-all' --height 50% "$@"
    }

    gf() {
      is_in_git_repo || return
      git -c color.status=always status --short |
      fzf-down -m --ansi --nth 1..,.. \
        --preview '(git diff --color=always -- {-1} | sed 1,4d; cat {-1}) | head -500' |
      cut -c4- | sed 's/.* -> //'
    }

    gb() {
      is_in_git_repo || return
      git branch -a --color=always | grep -v '/HEAD\s' | sort |
      fzf-down --ansi --multi --tac --preview-window right:70% \
        --preview 'git log --oneline --graph --date=short --pretty="format:%C(auto)%cd %h%d %s" $(sed s/^..// <<< {} | cut -d" " -f1) | head -'$LINES |
      sed 's/^..//' | cut -d' ' -f1 |
      sed 's#^remotes/##'
    }

    gh() {
      is_in_git_repo || return
      git log --date=short --format="%C(green)%C(bold)%cd %C(auto)%h%d %s (%an)" --graph --color=always |
      fzf-down --ansi --no-sort --reverse --multi --bind 'ctrl-s:toggle-sort' \
        --header 'Press CTRL-S to toggle sort' \
        --preview 'grep -o "[a-f0-9]\{7,\}" <<< {} | xargs git show --color=always | head -'$LINES |
      grep -o "[a-f0-9]\{7,\}"
    }

    join-lines() {
      local item
      while read item; do
        echo -n "${(q)item} "
      done
    }

    bind-git-helper() {
      local char
      for c in $@; do
        eval "fzf-g$c-widget() { local result=\$(g$c | join-lines); zle reset-prompt; LBUFFER+=\$result }"
        eval "zle -N fzf-g$c-widget"
        eval "bindkey '^g$c' fzf-g$c-widget"
      done
    }

    bindkey -r '^g'
    bind-git-helper f b h
    unset -f bind-git-helper
fi

#------------------------------------------------------------------------------
# fasd settings

# enable fasd
if command -v fasd &> /dev/null; then
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
# Somehow the order for output matters! logname > pwd > hostname
# Otherwise, there will be "Tramp" errors.
function eterm-update {
    echo -e "\033AnSiTu" "$LOGNAME"
    echo -e "\033AnSiTc" "$(pwd)"
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
}

if [[ $TERM =~ eterm ]]; then
    function chpwd {
        eterm-update
    }
    cd "$(pwd)"
fi

#------------------------------------------------------------------------------
# Homebrew settings
export HOMEBREW_NO_AUTO_UPDATE=true

#==============================================================================
# plugins
AUTO_SUGGESTIONS=$HOME/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh
if [[ -r $AUTO_SUGGESTIONS && $TERM =~ ".*256" ]]; then
    source $AUTO_SUGGESTIONS
    ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=030'
    ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=20 # do not trigger completion for large buffer
fi

#==============================================================================
# load other settings
if [[ -f ~/.zshrc-local ]]; then
    . ~/.zshrc-local
fi

# added by travis gem
[ -f /Users/jinzhouz/.travis/travis.sh ] && source /Users/jinzhouz/.travis/travis.sh
test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

#==============================================================================
#zprof # bottom of .zshrc
