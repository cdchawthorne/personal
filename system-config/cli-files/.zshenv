setopt BAD_PATTERN NO_HUP LOCAL_OPTIONS LOCAL_TRAPS NULL_GLOB
setopt NO_FUNCTION_ARG_ZERO RC_QUOTES EXTENDED_GLOB
typeset -U fpath
fpath=(${HOME}/.zsh/autoloads ${fpath})
EDITOR=vim
VISUAL=vim
CSCOPE_DB=${HOME}/utilities/databases/cscope.out
TERM=xterm-256color
TEXMFHOME=${HOME}/.dev/texmf

if [[ $(uname -n) != computer-of-destiny ]]; then
    LC_ALL=en_CA.UTF-8
    LANG=en_CA.UTF-8
    LANGUAGE=en_CA.UTF-8
fi

export EDITOR VISUAL CSCOPE_DB TERM TEXMFHOME LC_ALL LANG LANGUAGE
