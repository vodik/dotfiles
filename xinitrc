cd

start-pulseaudio-x11 &
xrdb -merge ~/.Xresources &
nitrogen --restore &
xsetroot -cursor_name left_ptr &

case ${2} in
  *)
    exec ck-launch-session dbus-launch ~/bin/xsession "$2"
    ;;
esac

# vi: ft=sh
