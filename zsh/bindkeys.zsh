bindkey "^[[A"   history-beginning-search-backward
bindkey "^[[B"   history-beginning-search-forward
bindkey "\e[1~"  beginning-of-line
bindkey "\e[4~"  end-of-line
bindkey "\e[7~"  beginning-of-line
bindkey "\e[8~"  end-of-line
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
