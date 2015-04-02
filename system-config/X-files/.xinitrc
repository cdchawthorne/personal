usermodmap=${HOME}/.Xmodmap
userresources=${HOME}/.Xresources
userprofile=${HOME}/.xprofile

# Taken from /etc/skel/.xinitrc
if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

[ -r ${userprofile} ] && . ${userprofile}

if [ -r "${usermodmap}" ]; then
    xmodmap "${usermodmap}"
fi

if [ -r "${userresources}" ]; then
    xrdb -merge "${userresources}"
fi

if [ -r "${HOME}/.xbindkeysrc" ]; then
    raine x xbindkeys
fi

# Get rid of those thrice-damnèd beeps
xset b off

feh --bg-max "$(shuf -n 1 ~/scratch/backs.txt)"

raine x xscreensaver -no-splash &

# jam_cmd='pgrep -u "$(whoami)" -f jam.sh &> /dev/null || ${HOME}/jam.sh &|'
# nerd_cmd='pgrep -u "$(whoami)" -f nerd.sh &> /dev/null || ${HOME}/nerd.sh &|'
# ssh nullsleep zsh -c "${jam_cmd}; ${nerd_cmd}" &

exec xmonad
# exec i3
