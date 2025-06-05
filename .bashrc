# ~/.bashrc

# Если оболочка неинтерактивная, ничего не делаем
[[ $- != *i* ]] && return

alias markupCS="bash /home/chillwcat/bsh/markupCS.sh"
alias flamegui="flameshot gui"
# История команд
HISTCONTROL=ignoreboth  # Игнорировать дубли и команды, начинающиеся с пробела
HISTSIZE=1000           # Размер истории в памяти
HISTFILESIZE=2000       # Размер файла истории
shopt -s histappend     # Добавлять в историю, а не перезаписывать

# Проверка размера окна после каждой команды
shopt -s checkwinsize

# Режим ** - подстановка для всех файлов и подкаталогов
shopt -s globstar

# Цветной вывод `ls`
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# Полезные алиасы
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

# Цветной prompt (PS1)
PS1='\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '

# Добавление пользовательских путей в PATH
if [ -d "$HOME/.local/bin" ]; then
    PATH="$HOME/.local/bin:$PATH"
fi

# Загрузка дополнительных настроек из ~/.bash_aliases, если файл существует
if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# Автодополнение команд
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi
