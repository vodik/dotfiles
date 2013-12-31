alias ls='ls -FNh --tabsize=0 --color=auto --show-control-chars --group-directories-first'
alias ll='ls -l'
alias la='ls -a'
alias lla='ls -la'
alias tree='tree -C'

alias netcfg='sudo netcfg'
alias netcfg-menu='sudo netcfg-menu'
alias wifi-select='sudo wifi-select'

# stderred support (https://github.com/albinoloverats/stderred)
alias _='LD_PRELOAD=/usr/lib/libstderred.so'

agent() {
  local -a envoy
  envoy=(envoy -t gpg-agent)

  case ${1:-start} in
    start) source <($envoy -p) ;;
    stop)  $envoy -K           ;;
    list)  $envoy -l           ;;
    print) $envoy -p           ;;
  esac
}

aurgrab() {
  curl -s "https://aur.archlinux.org/packages/${1:0:2}/$1/$1".tar.gz | tar -xzv
}

usecc() {
  local cc=$1

  if [[ -z $cc ]]; then
    [[ $CC = "gcc" ]] && cc="clang" || cc="gcc"
  fi

  case $cc in
    gcc)   export CC="gcc"   CXX="g++"          ;;
    clang) export CC="clang" CXX="clang++"      ;;
    *)     echo >&2 "Compiler $cc unrecognized" ;;
  esac
  echo "CC=$CC"
}
