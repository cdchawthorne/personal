setopt BAD_PATTERN NO_HUP LOCAL_OPTIONS LOCAL_TRAPS #NULL_GLOB
setopt NO_FUNCTION_ARG_ZERO RC_QUOTES EXTENDED_GLOB
typeset -U path fpath
path=(${HOME}/bin ${path})
fpath=(${HOME}/.zsh/autoloads ${HOME}/.zsh/completion ${fpath})
EDITOR=vim
VISUAL=vim
CSCOPE_DB=${HOME}/utilities/databases/cscope.out
TERM=xterm-256color
TEXMFHOME=${HOME}/.texmf
NVIM_TUI_ENABLE_CURSOR_SHAPE=1

if [[ $(uname -n) != computer-of-destiny ]]; then
    LC_ALL=en_CA.UTF-8
    LANG=en_CA.UTF-8
    LANGUAGE=en_CA.UTF-8
fi

export EDITOR VISUAL CSCOPE_DB TERM TEXMFHOME LC_ALL LANG LANGUAGE \
    NVIM_TUI_ENABLE_CURSOR_SHAPE
