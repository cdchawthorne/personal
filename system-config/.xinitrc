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
keepassxc shared/passwords/passwords.kdbx &
xinput set-prop 10 250 1
xset s off

xrandr --output DVI-D-0 --off --output HDMI-0 --mode 1920x1080 --pos 0x0 --rotate normal --output DP-0 --off --output DP-1 --off --output DP-2 --primary --mode 1920x1080 --pos 1920x0 --rotate normal --output DP-3 --off --output DP-4 --off --output DP-5 --off

exec i3
