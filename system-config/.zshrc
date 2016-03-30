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

if [[ $(hostname) == "subterfuge" ]]; then
    PROMPT='%~%# '
else
    PROMPT='%m:%~%# '
fi

PROMPT2='> '
KEYTIMEOUT=100

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

# # Navigation bindings
# function _new-tab { tmux new-window -c "#{pane_current_path}" &> /dev/null }
# function _last-tab { tmux last-window &> /dev/null }
# function _next-tab { tmux next-window &> /dev/null }
# function _previous-tab { tmux previous-window &> /dev/null }
# function _horizontal-split {
    # tmux split-window -v -c "#{pane_current_path}" &> /dev/null
# }
# function _vertical-split {
    # tmux split-window -h -c "#{pane_current_path}" &> /dev/null
# }
# function _window-up { tmux select-pane -U &> /dev/null }
# function _window-down { tmux select-pane -D &> /dev/null }
# function _window-left { tmux select-pane -L &> /dev/null }
# function _window-right { tmux select-pane -R &> /dev/null }
# function _tab1 { tmux select-window -t :1 &> /dev/null }
# function _tab2 { tmux select-window -t :2 &> /dev/null }
# function _tab3 { tmux select-window -t :3 &> /dev/null }
# function _tab4 { tmux select-window -t :4 &> /dev/null }
# function _tab5 { tmux select-window -t :5 &> /dev/null }
# function _tab6 { tmux select-window -t :6 &> /dev/null }
# function _tab7 { tmux select-window -t :7 &> /dev/null }
# function _tab8 { tmux select-window -t :8 &> /dev/null }
# function _tab9 { tmux select-window -t :9 &> /dev/null }
# function _tab10 { tmux select-window -t :10 &> /dev/null }
# function _move-window-up { tmux swap-pane -U &> /dev/null }
# function _move-window-down { tmux swap-pane -D &> /dev/null }
# function _move-window-left { tmux swap-pane -U &> /dev/null }
# function _move-window-right { tmux swap-pane -D &> /dev/null }
# function _swap-tab { tmux command-prompt "swap-window -t ':%%'" &> /dev/null }
# function _join-window { tmux command-prompt "join-pane -s ':%%'" &> /dev/null }
# function _break-window { tmux break-pane \; last-window &> /dev/null }

# zle -N _new-tab
# zle -N _last-tab
# zle -N _next-tab
# zle -N _previous-tab
# zle -N _horizontal-split
# zle -N _vertical-split
# zle -N _window-up
# zle -N _window-down
# zle -N _window-left
# zle -N _window-right
# zle -N _tab1
# zle -N _tab2
# zle -N _tab3
# zle -N _tab4
# zle -N _tab5
# zle -N _tab6
# zle -N _tab7
# zle -N _tab8
# zle -N _tab9
# zle -N _move-window-up
# zle -N _move-window-down
# zle -N _move-window-left
# zle -N _move-window-right
# zle -N _swap-tab
# zle -N _join-window
# zle -N _break-window

# bindkey -M vicmd 'sjc' _new-tab
# bindkey -M vicmd 'sjk' _last-tab
# bindkey -M vicmd 'sjl' _next-tab
# bindkey -M vicmd 'sjh' _previous-tab
# bindkey -M vicmd 'sjs' _horizontal-split
# bindkey -M vicmd 'sjv' _vertical-split
# bindkey -M vicmd 'sdk' _window-up
# bindkey -M vicmd 'sdj' _window-down
# bindkey -M vicmd 'sdh' _window-left
# bindkey -M vicmd 'sdl' _window-right
# bindkey -M vicmd 'sj1' _tab1
# bindkey -M vicmd 'sj2' _tab2
# bindkey -M vicmd 'sj3' _tab3
# bindkey -M vicmd 'sj4' _tab4
# bindkey -M vicmd 'sj5' _tab5
# bindkey -M vicmd 'sj^' _tab6
# bindkey -M vicmd 'sj&' _tab7
# bindkey -M vicmd 'sj*' _tab8
# bindkey -M vicmd 'sj(' _tab9
# bindkey -M vicmd 'sj)' _tab10
# bindkey -M vicmd 'sdK' _move-window-up
# bindkey -M vicmd 'sdJ' _move-window-down
# bindkey -M vicmd 'sdH' _move-window-left
# bindkey -M vicmd 'sdL' _move-window-right
# bindkey -M vicmd 'sj.' _swap-tab
# bindkey -M vicmd 'sjj' _join-window
# bindkey -M vicmd 'sjb' _break-window

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

umask 027

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
