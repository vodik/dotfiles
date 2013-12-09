cmdignore=(vim top htop weechat-ncurses tmux git systemctl loginctl)

function notify-send-precmd() {
  local retval=$?
  local cmd_stat cmd_time

  [[ -n $SSH_TTY ]] && return;
  [[ -z $cmd ]] && return;
  [[ ${cmdignore[(r)$cmd_basename]} == $cmd_basename ]] && return;

  (( cmd_time = $(date +%s) - $cmd_start ))
  (( $cmd_time < 10 )) && return;

  if (( $retval > 0 )); then
    cmd_stat="returning $retval"
  else
    cmd_stat="successfully"
  fi

  notify-send -i utilities-terminal -u low "$cmd_basename completed $cmd_stat" "\"$cmd\" took $cmd_time seconds"
  unset cmd cmd_basename cmd_start
}

function notify-send-preexec() {
  [[ -n $SSH_TTY ]] && return;

  cmd=$1
  cmd_basename=${${cmd:s/sudo //}[(ws: :)1]}
  cmd_start=$(date +%s)
}

precmd_functions+=( notify-send-precmd )
preexec_functions+=( notify-send-preexec )
