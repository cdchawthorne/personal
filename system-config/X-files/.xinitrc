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

set-random-background || feh --bg-max ${HOME}/media/pictures/backgrounds/error.jpg
xscreensaver -no-splash &

chromium &

# Get rid of those thrice-damnèd beeps
xset b off

exec xmonad
#exec i3
