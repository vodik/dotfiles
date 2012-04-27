export BROWSER="firefox"
export EDITOR="vim"
export VISUAL="vim"
export PAGER="less"

export DISPLAY=${DISPLAY:-:0}

export GREP_OPTIONS="--color=auto"
export GREP_COLOR="4;1;31"

[[ -f ~/etc/dircolors ]] && eval $(dircolors ~/etc/dircolors)

export LESS_TERMCAP_mb=$'\E[01;31m'  # begin blinking
export LESS_TERMCAP_md=$'\E[01;31m'  # begin bold
export LESS_TERMCAP_me=$'\E[0m'      # end mode
export LESS_TERMCAP_so=$'\E[01;36m'  # begin standout-mode
export LESS_TERMCAP_se=$'\E[0m'      # end standout-mode
export LESS_TERMCAP_us=$'\E[00;34m'  # begin underline
export LESS_TERMCAP_ue=$'\E[0m'      # end underline

export ABSROOT="$HOME/.build/abs"

[[ -d /srv/http/archlinux/vodik/os ]] && \
  hash -d repo=/srv/http/archlinux/vodik/os

eval $(keychain start) >/dev/null

function precmd {
  _title "%15<..<%~%<<" "%n@%m: %~"
}

function preexec {
  local cmd=${1[(wr)^(*=*|sudo|ssh|-*)]}
  _title "%100>...>$2%<<" "$cmd"
}

function _title {
  case $TERM in
    screen*)
      print -Pn "\ek$1:q\e\\"
      ;;
    xterm*|rxvt*)
      print -Pn "\e]2;$2:q\a"
      print -Pn "\e]1;$1:q\a"
      ;;
  esac
}
