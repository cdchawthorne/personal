setopt BAD_PATTERN NO_HUP LOCAL_OPTIONS LOCAL_TRAPS RC_QUOTES
setopt NO_FUNCTION_ARG_ZERO EXTENDED_GLOB PROMPT_SUBST TRANSIENT_RPROMPT
typeset -U path fpath
path=(${HOME}/bin ${path})
fpath=(${HOME}/.zsh/autoloads ${HOME}/.zsh/completion ${fpath})
EDITOR=nvim
VISUAL=nvim
CSCOPE_DB=${HOME}/utilities/databases/cscope.out
TERM=xterm-256color
TEXMFHOME=${HOME}/.texmf
FZF_DEFAULT_COMMAND='cat ~/utilities/fzf_db'
FZF_ALT_C_COMMAND='find * .* -regextype posix-extended \( -regex '
FZF_ALT_C_COMMAND+='''.local|builds|.cache'' -o -regex ''.*/\.git'' \) -prune '
FZF_ALT_C_COMMAND+='-o -type d -print 2>/dev/null'

if [[ $(uname -n) != computer-of-destiny ]]; then
    LC_ALL=en_CA.UTF-8
    LANG=en_CA.UTF-8
    LANGUAGE=en_CA.UTF-8
fi

export EDITOR VISUAL CSCOPE_DB TERM TEXMFHOME LC_ALL LANG LANGUAGE \
    FZF_DEFAULT_COMMAND FZF_ALT_C_COMMAND
