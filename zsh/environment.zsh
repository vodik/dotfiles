export DISPLAY=${DISPLAY:-:0}

export GREP_OPTIONS="--color=auto"
export GREP_COLOR="4;1;31"

[[ -f ~/etc/dircolors/dircolors ]] && eval $(dircolors ~/etc/dircolors/dircolors)

export LESS_TERMCAP_mb=$'\E[01;31m'  # begin blinking
export LESS_TERMCAP_md=$'\E[01;31m'  # begin bold
export LESS_TERMCAP_me=$'\E[0m'      # end mode
export LESS_TERMCAP_so=$'\E[01;36m'  # begin standout-mode
export LESS_TERMCAP_se=$'\E[0m'      # end standout-mode
export LESS_TERMCAP_us=$'\E[00;34m'  # begin underline
export LESS_TERMCAP_ue=$'\E[0m'      # end underline

export ABSROOT="$HOME/build/abs"
[[ -d /srv/http/archlinux/vodik/os ]] && \
  hash -d repo=/srv/http/archlinux/vodik/os

if (( $UID != 0 && $+commands[envoy] )); then
  envoy 2>/dev/null
  eval $(envoy -p)
fi

[[ $TERM == xterm ]] && export TERM=xterm-256color

case $TERM in
  termite|vte*|xterm*|rxvt*)

    function precmd {
      print -Pn '\e];%n@%m %~\a'
    }

    function preexec {
      local cmd=${1[(wr)^(*=*|sudo|ssh|-*)]}
      print -Pn "\e];$cmd:q\a"
    }

    ;;
esac
