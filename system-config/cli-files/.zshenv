setopt BAD_PATTERN NO_HUP LOCAL_OPTIONS LOCAL_TRAPS NULL_GLOB
setopt NO_FUNCTION_ARG_ZERO RC_QUOTES EXTENDED_GLOB
typeset -U fpath
fpath=(${HOME}/.zsh/autoloads ${fpath})
EDITOR=vim
VISUAL=vim
CSCOPE_DB=${HOME}/utilities/databases/cscope.out
TERM=xterm-256color
TEXMFHOME=${HOME}/.dev/texmf
VIMRUNTIME=${HOME}/builds/vim/runtime
export EDITOR VISUAL CSCOPE_DB TERM TEXMFHOME VIMRUNTIME
