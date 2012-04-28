#!/bin/zsh

autoload -U colors && colors
autoload -U compinit && compinit

# smart urls
autoload -U url-quote-magic
zle -N self-insert url-quote-magic

# changing directories
setopt auto_cd
setopt auto_pushd
setopt cdable_vars
setopt chase_links
setopt pushd_ignore_dups
setopt pushd_silent
setopt pushd_to_home

setopt auto_name_dirs
setopt multios
setopt extended_glob
setopt prompt_subst
setopt long_list_jobs

unsetopt clobber
unsetopt flowcontrol

for snippets ($HOME/etc/zsh/*.zsh(N))
  source $snippets
