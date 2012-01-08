if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

xrdb -merge ~/.Xresources &
nitrogen --restore &
xsetroot -cursor_name left_ptr &

case ${2} in
  *)
    cd "$HOME"
    exec ck-launch-session "${2-xmonad}"
    ;;
esac

# vi: ft=sh
