# usermodmap=${HOME}/.Xmodmap
usermodmap=${HOME}/.Xmodmap-ext
# usermodmap=${HOME}/.Xmodmap-gaming
userprofile=${HOME}/.xprofile
userresources=${HOME}/.Xresources

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

[[ -r ${userprofile} ]] && . ${userprofile}
[[ -r ${usermodmap} ]] && xmodmap ${usermodmap}
[[ -r ${userresources} ]] && xrdb -merge ${userresources}

pere random

xscreensaver -no-splash &
sxhkd &

# Get rid of those thrice-damnèd beeps
xset b off

firefox &
exec i3
