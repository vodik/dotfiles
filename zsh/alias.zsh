alias ls='ls -FNh --tabsize=0 --color=auto --show-control-chars --group-directories-first'
alias ll='ls -l'
alias la='ls -a'
alias lla='ls -la'
alias tree='tree -A'

alias please='sudo $(history -n -1)'

# stderred support (https://github.com/albinoloverats/stderred)
alias _='LD_PRELOAD=/usr/lib/stderred.so'
