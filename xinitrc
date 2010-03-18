#!/bin/bash

enabledual()
{
	width=`xrandr | grep VGA | cut -c16-19`
	echo "Width $width"
	if [[ "$width" == "1024" ]]
	then
		xrandr --output LVDS1 --mode 1280x768  --pos 0x0    --rotate normal \
			   --output VGA1  --mode 1280x1024 --pos 1280x0 --rotate normal \
			   --output TV1   --off
	fi
}

start_xfce4() {
	exec xfce4-session
}

start_dwm() {
	enabledual

	urxvtd -q -f -o
	nitrogen --restore
	/home/simongmzlj/.dwm/title &
	xcompmgr &
	exec dwm
}

start_vbox() {
	vm="${@:-Windows 7}"
	exec VirtualBox --startvm "$vm"
}

start_xbmc() {
	exec xbmc
}

start_goo() {
	exec worldofgoo-demo
}

start_${2:-dwm} "${@:3}"
