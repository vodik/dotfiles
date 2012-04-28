alias ls="ls -FNh --tabsize=0 --color=auto --show-control-chars --group-directories-first"
alias ll="ls -l"
alias la="ls -a"
alias lp="ls -a --color=always | $PAGER -R"

alias vir="vim --servername vim --remote-tab-silent"

alias history="fc -l 1"

# stderred support (https://github.com/albinoloverats/stderred)
alias _='LD_PRELOAD=/usr/lib/stderred.so'
