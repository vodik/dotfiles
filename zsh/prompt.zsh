local pmt="red"

(( EUID == 0 )) && pmt="blue"
[[ -n $SSH_CONNECTION ]] && pmt="green"

PROMPT="%{$fg[$pmt]%}[%0~] "

if (( EUID == 0 )); then
  PROMPT+="%{$fg[red]%}##%{$reset_color%} "
else
  PROMPT+="%{$fg[blue]%}//%{$reset_color%} "
fi

unset pmt

# RPROMPT="$(git_prompt_info)"

# ZSH_THEME_GIT_PROMPT_PREFIX="%{$fg[green]%}["
# ZSH_THEME_GIT_PROMPT_SUFFIX="]%{$reset_color%}"
# ZSH_THEME_GIT_PROMPT_DIRTY=" %{$fg[red]%}*%{$fg[green]%}"
# ZSH_THEME_GIT_PROMPT_CLEAN=""
