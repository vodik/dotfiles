if (( EUID == 0 )); then
  PROMPT="%{$fg[blue]%}[%0~] %{$fg[red]%}##%{$reset_color%} "
else
  PROMPT="%{$fg[red]%}[%0~] %{$fg[blue]%}//%{$reset_color%} "
fi

# RPROMPT="$(git_prompt_info)"

# ZSH_THEME_GIT_PROMPT_PREFIX="%{$fg[green]%}["
# ZSH_THEME_GIT_PROMPT_SUFFIX="]%{$reset_color%}"
# ZSH_THEME_GIT_PROMPT_DIRTY=" %{$fg[red]%}*%{$fg[green]%}"
# ZSH_THEME_GIT_PROMPT_CLEAN=""
