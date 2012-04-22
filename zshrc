#!/bin/zsh

autoload colors && colors

for snippets ($HOME/etc/zsh/*.zsh(N))
  source $snippets

setopt auto_cd
setopt multios
setopt cdablevarS

setopt prompt_subst

# smart urls
autoload -U url-quote-magic
zle -N self-insert url-quote-magic

# jobs
setopt long_list_jobs
