if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

cd "$HOME"
xrdb -merge ~/.Xresources &
nitrogen --restore &

case ${2} in
  *)
    exec ck-launch-session "${2-xmonad}"
    ;;
esac

# vi: ft=sh
