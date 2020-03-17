usermodmap=${HOME}/.Xmodmap
userprofile=${HOME}/.xprofile
userresources=${HOME}/.Xresources

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

[[ -r ${userprofile} ]] && . ${userprofile}
[[ -r "${usermodmap}" ]] && xmodmap ${usermodmap}
[[ -r "${userresources}" ]] && xrdb -merge ${userresources}

pere random

sxhkd &
keepassxc .passwords/passwords.kdbx &
protonmail-bridge --no-window &
xinput set-prop 10 250 1
xset s off

exec i3
