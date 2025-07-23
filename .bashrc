# ~/.bashrc

# Если оболочка неинтерактивная, ничего не делаем
[[ $- != *i* ]] && return

alias flamegui="flameshot gui"
alias gf="~/repos/gf/gf2"
# alias lnbin="sudo bash ~/bsh/lnbin.sh"
#
# The history command
HISTCONTROL=ignoreboth  # the ignore clones
HISTSIZE=1000           # size history
HISTFILESIZE=2000       # size files
shopt -s histappend     # add to history, don't overwrite

shopt -s checkwinsize

shopt -s globstar

# color output
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# Collest aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'
alias ..='cd ..'
alias ...='cd ../..'
alias cp='cp -i'        # Запрос перед перезаписью
alias mv='mv -i'        # Запрос перед перезаписью
alias rm='rm -I'        # Запрос перед удалением многих файлов
alias df='df -h'        # Человеко-читаемые размеры
alias du='du -h'        # Человеко-читаемые размеры
alias mkdir='mkdir -p'  # Создавать родительские каталоги при необходимости

# color prompt (PS1)
PS1='\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '

# add user path to PATH
if [ -d "$HOME/.local/bin" ]; then
    PATH="$HOME/.local/bin:$PATH"
fi

# loading the extra settings from ~/.bash_aliases, if files is exist
if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# autocomplete
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi
. "$HOME/.cargo/env"
