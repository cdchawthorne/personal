# (Originally) taken from http://haskell.org/haskellwiki/Xmonad/Config_archive/John_Goerzen's_Configuration
# (It really doesn't bear much of a resemblance anymore.)

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

[[ -r ${userprofile} ]] && . ${userprofile}

if [ -r "${usermodmap}" ]; then
    xmodmap "${usermodmap}"
fi

if [ -r "${userresources}" ]; then
    xrdb -merge "${userresources}"
fi

if [ -r "${HOME}/.xbindkeysrc" ]; then
    xbindkeys
fi

# Set background or draw attention to the error
pere random

# Turn on xscreensaver
xscreensaver -no-splash &

# Get rid of those thrice-damnèd beeps
xset b off

firefox &

muteExp1='s_^.*\[on\]$_Unmuted_g'
muteExp2='s_^.*\[off\]$_Muted_g'
while true; do
    time="$(date +%H:%M)"
    free_ram="$(free -h | awk '/Mem/ {print $4;}')"
    bat="$(acpi -b | awk '{print $4 " " $5;}' | tr -d ',')"
    date="$(date +%Y-%m-%d)"
    vol="$(amixer get Master | tail -1 | sed 's/.*\[\([0-9]*%\)\].*/\1/')"
    muted="$(amixer get Master | tail -1 | sed -re ${muteExp1} -e ${muteExp2})"

    right="${bat} | ${free_ram} | ${vol} (${muted}) | ${date}"
    xsetroot -name "$(printf "%s %80s" ${time} "${right}")"
    sleep 1s
done &

exec ~/scratch/dwm/dwm
# exec i3
# exec xmonad
