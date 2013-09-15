# Lines configured by zsh-newuser-install
HISTFILE=~/.zsh_history
HISTSIZE=1000000
SAVEHIST=1000000
setopt appendhistory extendedglob notify
unsetopt beep
bindkey -v
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/cdchawthorne/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

setopt EXTENDED_HISTORY INC_APPEND_HISTORY
setopt HIST_IGNORE_DUPS HIST_IGNORE_SPACE HIST_NO_STORE
setopt NO_BEEP

if [[ $(uname -n) == "computer-of-destiny" ]]; then
    PROMPT='%~%# '
else
    PROMPT='%m:%~%# '
fi

PROMPT2='> '
KEYTIMEOUT=20

export zsh_parent_process="$(ps --no-headers -o comm $(ps --no-headers -o ppid | head -n 1))"
if [[ -n ${STY} ]]; then
    function zle-keymap-select {
        if [[ ${KEYMAP} == "vicmd" ]]; then
            echo -ne '\eP\e[1 q\e\'
        elif [[ ${KEYMAP} == "main" ]]; then
            echo -ne '\eP\e[3 q\e\'
        fi
    }
    function zle-line-init { 
        echo -ne '\eP\e[3 q\e\'
    }
    zle -N zle-keymap-select
    zle -N zle-line-init
    alias vim='echo -ne ''\eP\e[1 q\e\''; vim'
    alias svim='echo -ne ''\eP\e[1 q\e\''; sudoedit'
elif [[ -n ${DISPLAY} || ${zsh_parent_process} == "sshd" ]]; then
    function zle-keymap-select {
        if [[ ${KEYMAP} == "vicmd" ]]; then
            echo -ne '\e[1 q'
        elif [[ ${KEYMAP} == "main" ]]; then
            echo -ne '\e[3 q'
        fi
    }
    function zle-line-init { 
        echo -ne '\e[3 q'
    }
    zle -N zle-keymap-select
    zle -N zle-line-init
    alias vim='echo -ne ''\e[1 q''; vim'
    alias svim='echo -ne ''\e[1 q''; sudoedit'
else
    function zle-keymap-select {
        if [[ ${KEYMAP} == "vicmd" ]]; then
            echo -ne '\e[?6c'
        elif [[ ${KEYMAP} == "main" ]]; then
            echo -ne '\e[?2c'
        fi
    }
    function zle-line-init {
        echo -ne '\e[?2c'
    }
    zle -N zle-keymap-select
    zle -N zle-line-init
    alias vim='echo -ne ''\e[?6c''; vim'
    alias svim='echo -ne ''\e[?6c''; sudoedit'
fi

autoload up-line-or-beginning-search
autoload down-line-or-beginning-search
zle -N up-line-or-beginning-search 
zle -N down-line-or-beginning-search
zstyle ':zle:*-line-or-beginning-search' leave-cursor True

bindkey -M viins "fj" vi-cmd-mode
bindkey -M viins "Fj" vi-cmd-mode
bindkey -M viins "fJ" vi-cmd-mode
bindkey -M viins "FJ" vi-cmd-mode
bindkey -M viins "jf" vi-cmd-mode
bindkey -M viins "Jf" vi-cmd-mode
bindkey -M viins "jF" vi-cmd-mode
bindkey -M viins "JF" vi-cmd-mode
bindkey -M viins "^U" kill-whole-line
bindkey -M viins "^K" kill-whole-line
bindkey -M viins "^R" history-incremental-search-backward
bindkey -M viins '^[[3~' vi-delete-char

bindkey -M vicmd "k" history-beginning-search-backward
bindkey -M vicmd "j" history-beginning-search-forward
bindkey -M vicmd '^[[3~' vi-delete-char

zle -A .backward-kill-word vi-backward-kill-word
zle -A .backward-delete-char vi-backward-delete-char
zle -A .kill-line vi-kill-line

# Allow <C-q> and <C-s> to work in rtorrent
stty stop undef
stty start undef

if [[ -r ${HOME}/.zsh_aliases ]]; then
    . ${HOME}/.zsh_aliases
fi

if [[ -r ${HOME}/.zsh_autoload ]]; then
    . ${HOME}/.zsh_autoload
fi
