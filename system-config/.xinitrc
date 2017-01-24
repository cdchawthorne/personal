# usermodmap=${HOME}/.Xmodmap
usermodmap=${HOME}/.Xmodmap-ext
# usermodmap=${HOME}/.Xmodmap-gaming
userprofile=${HOME}/.xprofile

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

[[ -r ${userprofile} ]] && . ${userprofile}

if [ -r "${usermodmap}" ]; then
    xmodmap "${usermodmap}"
fi

pere random

xscreensaver -no-splash &
sxhkd &

# Get rid of those thrice-damnèd beeps
xset b off

firefox &
exec i3
