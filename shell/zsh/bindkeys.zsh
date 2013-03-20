autoload zsh/terminfo

# bg on empty line, push-input on non-empty line
fancy-ctrl-z() {
  if [[ $#BUFFER -eq 0 ]]; then
    bg
    zle redisplay
  else
    zle push-input
  fi
}; zle -N fancy-ctrl-z

# vi editing mode
bindkey -v

# shift-tab
if [[ -n $terminfo[kcbt] ]]; then
  bindkey "$terminfo[kcbt]" reverse-menu-complete
fi

# do history expansion on space
bindkey ' ' magic-space

# delete
if [[ -n $terminfo[kdch1] ]]; then
  bindkey          "$terminfo[kdch1]" delete-char
  bindkey -M vicmd "$terminfo[kdch1]" vi-delete-char
fi

# insert
if [[ -n $terminfo[kich1] ]]; then
  bindkey          "$terminfo[kich1]" overwrite-mode
  bindkey -M vicmd "$terminfo[kich1]" vi-insert
fi

# home
if [[ -n $terminfo[khome] ]]; then
  bindkey          "$terminfo[khome]" vi-beginning-of-line
  bindkey -M vicmd "$terminfo[khome]" vi-beginning-of-line
fi

# end
if [[ -n $terminfo[kend] ]]; then
  bindkey          "$terminfo[kend]" vi-end-of-line
  bindkey -M vicmd "$terminfo[kend]" vi-end-of-line
fi

# backspace (and <C-h>)
if [[ -n $terminfo[kbs] ]]; then
  bindkey          "$terminfo[kbs]" backward-delete-char
  bindkey -M vicmd "$terminfo[kbs]" backward-char
fi

bindkey           '^H' backward-delete-char
bindkey -M vicmd  '^H' backward-char

# page up (and <C-b> in vicmd)
if [[ -n $terminfo[kpp] ]]; then
  bindkey          "$terminfo[kpp]" beginning-of-buffer-or-history
  bindkey -M vicmd "$terminfo[kpp]" beginning-of-buffer-or-history
fi

bindkey -M vicmd '^B' beginning-of-buffer-or-history

# page down (and <C-f> in vicmd)
if [[ -n $terminfo[knp] ]]; then
  bindkey          "$terminfo[knp]" end-of-buffer-or-history
  bindkey -M vicmd "$terminfo[knp]" end-of-buffer-or-history
fi

bindkey -M vicmd '^F' end-of-buffer-or-history

# up arrow (history search)
if [[ -n $terminfo[kcuu1] ]]; then
  bindkey          "$terminfo[kcuu1]" history-beginning-search-backward
  bindkey -M vicmd "$terminfo[kcuu1]" history-beginning-search-backward
fi

# down arrow (history search)
if [[ -n $terminfo[kcud1] ]]; then
  bindkey          "$terminfo[kcud1]" history-beginning-search-forward
  bindkey -M vicmd "$terminfo[kcud1]" history-beginning-search-forward
fi

# left arrow (whichwrap)
if [[ -n $terminfo[kcub1] ]]; then
  bindkey          "$terminfo[kcub1]" backward-char
  bindkey -M vicmd "$terminfo[kcub1]" backward-char
fi

# right arrow (whichwrap)
if [[ -n $terminfo[kcuf1] ]]; then
  bindkey          "$terminfo[kcuf1]" forward-char
  bindkey -M vicmd "$terminfo[kcuf1]" forward-char
fi

# shift-left
if [[ -n $terminfo[kLFT] ]]; then
  bindkey          "$terminfo[kLFT]" vi-backward-word
  bindkey -M vicmd "$terminfo[kLFT]" vi-backward-word
fi

# shift-right
if [[ -n $terminfo[kRIT] ]]; then
  bindkey          "$terminfo[kRIT]" vi-forward-word
  bindkey -M vicmd "$terminfo[kRIT]" vi-forward-word
fi

# ctrl-left
bindkey '^[[1;5D' vi-backward-blank-word
bindkey '^[[1;5C' vi-forward-blank-word
bindkey '^i'      expand-or-complete-prefix
bindkey '^W'      backward-kill-word

bindkey -M vicmd 'h'  backward-char
bindkey -M vicmd 'l'  forward-char
bindkey -M vicmd '^R' redo
bindkey -M vicmd 'u'  undo
bindkey -M vicmd 'ga' what-cursor-position
bindkey -M vicmd 'v'  edit-command-line

# fancy <C-z>
bindkey          '^Z' fancy-ctrl-z
bindkey -M vicmd '^Z' fancy-ctrl-z

# buffer stack access
bindkey '^w' push-line
bindkey '^e' get-line
