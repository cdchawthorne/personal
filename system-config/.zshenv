setopt BAD_PATTERN NO_HUP LOCAL_OPTIONS LOCAL_TRAPS NULL_GLOB
setopt NO_FUNCTION_ARG_ZERO RC_QUOTES EXTENDED_GLOB
typeset -U path fpath
path=(${HOME}/bin ${path})
fpath=(${HOME}/.zsh/autoloads ${HOME}/.zsh/completion ${fpath})
EDITOR=vim
VISUAL=vim
CSCOPE_DB=${HOME}/utilities/databases/cscope.out
TERM=xterm-256color
TEXMFHOME=${HOME}/.texmf
# TODO: locate
FZF_DEFAULT_COMMAND='find * .* -regextype posix-extended -regex '
FZF_DEFAULT_COMMAND+='''.local|builds|.cache'' -prune -o \( -type f -o -type l'
FZF_DEFAULT_COMMAND+=' \) -a \! -regex ''.*\.(pdf|mp3|m4a|flac|png|jpg|gif|'
FZF_DEFAULT_COMMAND+='class|o|hi|localstorage|localstorage-journal|svn-base|'
FZF_DEFAULT_COMMAND+='dyn_hi)$'' -print 2>/dev/null'

if [[ $(uname -n) != computer-of-destiny ]]; then
    LC_ALL=en_CA.UTF-8
    LANG=en_CA.UTF-8
    LANGUAGE=en_CA.UTF-8
fi

export EDITOR VISUAL CSCOPE_DB TERM TEXMFHOME LC_ALL LANG LANGUAGE \
    FZF_DEFAULT_COMMAND