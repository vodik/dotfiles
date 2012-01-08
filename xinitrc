cd

start-pulseaudio-x11 &
xrdb -merge ~/.Xresources &
nitrogen --restore &
xsetroot -cursor_name left_ptr &

case ${2} in
  *)
    exec ck-launch-session dbus-launch "${2-xmonad}"
    ;;
esac

# vi: ft=sh
