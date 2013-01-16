alias ls='ls -FNh --tabsize=0 --color=auto --show-control-chars --group-directories-first'
alias ll='ls -l'
alias la='ls -a'
alias lla='ls -la'
alias tree='tree -A'

alias agent='source <(envoy -p)'
alias please='sudo $(history -n -1)'

alias netcfg='sudo netcfg'
alias netcfg-menu='sudo netcfg-menu'
alias wifi-select='sudo wifi-select'

alias startx='exec \startx'

alias x='exec startx'

# stderred support (https://github.com/albinoloverats/stderred)
alias _='LD_PRELOAD=/usr/lib/libstderred.so'

usegcc() {
  [[ $CC  == gcc ]] && export CC=clang    || export CC=gcc
  [[ $CXX == g++ ]] && export CXX=clang++ || export CXX=g++
}
