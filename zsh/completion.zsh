_force_rehash() {
  (( CURRENT = 1 )) && rehash
  return 1
}

zstyle ':completion:*' completer _oldlist _expand _force_rehash _complete
