# colour env settings
[[ -f ~/etc/dircolors/dircolors ]] && \
  source <(dircolors ~/etc/dircolors/dircolors)

export SUDO_PROMPT=$'\e[31mSUDO\e[m password for \e[34m%p\e[m: '
export LESS_TERMCAP_mb=$'\e[01;31m'  # begin blinking
export LESS_TERMCAP_md=$'\e[01;31m'  # begin bold
export LESS_TERMCAP_me=$'\e[0m'      # end mode
export LESS_TERMCAP_so=$'\e[01;36m'  # begin standout-mode
export LESS_TERMCAP_se=$'\e[0m'      # end standout-mode
export LESS_TERMCAP_us=$'\e[00;36m'  # begin underline
export LESS_TERMCAP_ue=$'\e[0m'      # end underline

export ABSROOT="$HOME/build/abs"
[[ -d /srv/http/archlinux/vodik/os ]] && \
  hash -d repo=/srv/http/archlinux/vodik/os

[[ $DESKTOP_SESSION != 'gnome' ]] && (( $UID != 0 && $+commands[envoy] )) && \
  source <(envoy -t gpg-agent -p)

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
