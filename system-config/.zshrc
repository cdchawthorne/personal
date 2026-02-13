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

if [[ -z $SSH_TTY ]]; then
    PROMPT='%~%# '
else
    PROMPT='%m:%~%# '
fi

PROMPT2='> '
KEYTIMEOUT=100

vim_ins_mode="%F{red}INS%f"
vim_cmd_mode="%F{green}CMD%f"
vim_mode=$vim_ins_mode

function zle-keymap-select {
  vim_mode="${${KEYMAP/vicmd/${vim_cmd_mode}}/(main|viins)/${vim_ins_mode}}"
  zle reset-prompt
}
zle -N zle-keymap-select

RPROMPT='${vim_mode}'

function TRAPINT() {
  vim_mode=$vim_ins_mode
  return $(( 128 + $1 ))
}

function zle-line-finish {
  vim_mode=$vim_ins_mode
}
zle -N zle-line-finish

autoload up-line-or-beginning-search
autoload down-line-or-beginning-search
autoload -U tetris
zle -N up-line-or-beginning-search 
zle -N down-line-or-beginning-search
zle -N tetris
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
bindkey -M vicmd 'skt' tetris
bindkey -M vicmd ')' vi-beginning-of-line

zle -A .backward-kill-word vi-backward-kill-word
zle -A .backward-delete-char vi-backward-delete-char
zle -A .kill-line vi-kill-line

# Complete ssh hostnames from the Host of .ssh/config before the hostnames
# From https://www.reddit.com/r/zsh/comments/dw3h5n/zsh_ssh_autocomplete_host_instead_of_hostname/
zstyle ':completion:*:(ssh|scp|sftp):*' hosts $hosts

# Allow <C-q> and <C-s> to work in rtorrent
stty stop undef
stty start undef

if [[ -r ${HOME}/.zsh_aliases ]]; then
    . ${HOME}/.zsh_aliases
fi

if [[ -r ${HOME}/.zsh_autoload ]]; then
    . ${HOME}/.zsh_autoload
fi

umask 027

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
