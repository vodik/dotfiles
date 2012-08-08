setopt prompt_subst
autoload -Uz vcs_info

zstyle ':vcs_info:*'              enable        git cvs svn
zstyle ':vcs_info:*'              actionformats '%F{5}(%f%s%F{5})%F{3}-%F{5}[%F{2}%b%F{3}|%F{1}%a%F{5}]%f '
zstyle ':vcs_info:*'              formats       '%F{5}(%f%s%F{5})%F{3}-%F{5}[%F{2}%b%F{5}]%f '
zstyle ':vcs_info:(sv[nk]|bzr):*' branchformat  '%b%F{1}:%F{3}%r'

vcs_info_wrapper() {
  vcs_info
  [[ -n "$vcs_info_msg_0_" ]] && echo "%{$fg[grey]%}${vcs_info_msg_0_/ /}%{$reset_color%}"
}

set_prompt() {
  local pmt="red"

  (( EUID == 0 ))          && pmt="blue"
  [[ -n $SSH_CONNECTION ]] && pmt="magenta"

  PROMPT="%{$fg[$pmt]%}[%0~] "
  (( EUID == 0 )) && PROMPT+="%{$fg[red]%}##%{$reset_color%} " \
                  || PROMPT+="%{$fg[blue]%}//%{$reset_color%} "

  RPROMPT=$'$(vcs_info_wrapper)'
  RPROMPT+="%{$fg[yellow]%}%(?.. %?)%{$reset_color%}"
}

set_prompt
unset set_promt
