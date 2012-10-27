autoload zsh/terminfo

# up
if [[ -n $terminfo[cuu1] ]]; then
  bindkey "$terminfo[cuu1]" history-beginning-search-backward
  bindkey -M vicmd "$terminfo[cuu1]" history-beginning-search-backward
fi

# down
if [[ -n $terminfo[cud1] ]]; then
  bindkey "$terminfo[cud1]" history-beginning-search-forward
  bindkey -M vicmd "$terminfo[cud1]" history-beginning-search-forward
fi

bindkey "^[[A" history-beginning-search-backward
bindkey "^[[B" history-beginning-search-forward

# home
if [[ -n $terminfo[khome] ]]; then
  bindkey "$terminfo[khome]" vi-beginning-of-line
  bindkey -M vicmd "$terminfo[khome]" vi-beginning-of-line
fi

# end
if [[ -n $terminfo[kend] ]]; then
  bindkey "$terminfo[kend]" vi-end-of-line
  bindkey -M vicmd "$terminfo[kend]" vi-end-of-line
fi

bindkey "\e[5~"  beginning-of-history
bindkey "\e[6~"  end-of-history
bindkey "\e[3~"  delete-char
bindkey "\e[2~"  quoted-insert
bindkey "\e\e[C" forward-word
bindkey "\e\e[D" backward-word
bindkey "\e[5C"  forward-word
bindkey "\e[5D"  backward-word
bindkey "\eOc"   emacs-forward-word
bindkey "\eOd"   emacs-backward-word
bindkey "^H"     backward-delete-word

# completion in the middle of a line
bindkey '^i' expand-or-complete-prefix

# buffer stack access
bindkey '^w' push-line
bindkey '^e' get-line
