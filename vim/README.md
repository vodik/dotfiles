Dotvim
======

This is my Vim configuration. I finally have things how I like them.
Feel free use, tweak, or modify.

Mappings
--------

I have a few custom mappings. Listed here are some of the more notable
changes.

Correction for some common typo.

```
command! Q q
command! W w
command! Qa qa
command! Wq wq
command! Wa wa
command! Wqa wqa
```

Better window movement.

```
nmap <silent> <C-H> :winc h<CR>
nmap <silent> <C-J> :winc j<CR>
nmap <silent> <C-K> :winc k<CR>
nmap <silent> <C-L> :winc l<CR>
```

Toggle folds on spacebar.

```
nnoremap <silent> <Space> @=(foldlevel('.')?'za':'l')<CR>
vnoremap <Space> zf
```

Bindings for make.

```
nnoremap <F5>    :make<CR>
nnoremap <S-F5>  :make %<<CR>
nnoremap <S-F6>  :! ./%<CR>
```

Syntax
------

Packages along is a syntax file for [nginx][] configuration files and
an improved version of the [haskell][] syntax file which has a richer
markup.

  [nginx]: https://github.com/simongmzlj/dotvim/blob/master/syntax/nginx.vim
  [haskell]: https://github.com/simongmzlj/dotvim/blob/master/syntax/haskell.vim

Color Schemes
-------------

Included is [bclear][]. Not originally mine but I've been working on
it and plan to transform it into my vision. Its currently a work in
progress.

  [bclear]: https://github.com/simongmzlj/dotvim/blob/master/colors/bclear.vim

Plugins
-------

This configuration makes heavy use of [pathogen][] to manage plugins.

The important plugins I rely on:

- [a][]
- [abolish][]
- [commentary][]
- [csapprox][]
- [fugitive][]
- [gundo][]
- [html5-syntax][]
- [NERDtree][]
- [pandoc][]
- [repeat][]
- [snipmate][]
- [supertab][]
- [surround][]
- [tabular][]
- [tagbar][]

I have and use my own fork of the [snipmate-snippets][] repository.
Mostly just to tweak the coding style of the C snippets.

  [pathogen]: https://github.com/tpope/vim-pathogen
  [a]: https://github.com/vim-scripts/a.vim
  [abolish]: https://github.com/vim-scripts/abolish.vim
  [commentary]: https://github.com/tpope/vim-commentary
  [csapprox]: https://github.com/godlygeek/csapprox
  [fugitive]: https://github.com/tpope/vim-fugitive
  [gundo]: https://github.com/sjl/gundo.vim
  [html5-syntax]: https://github.com/othree/html5-syntax.vim
  [NERDtree]: https://github.com/vim-scripts/The-NERD-tree
  [pandoc]: https://github.com/vim-pandoc/vim-pandoc
  [repeat]: https://github.com/tpope/vim-repeat
  [snipmate]: https://github.com/garbas/vim-snipmate
  [supertab]: https://github.com/ervandew/supertab
  [surround]: https://github.com/tpope/vim-surround
  [tabular]: https://github.com/godlygeek/tabular
  [tagbar]: https://github.com/majutsushi/tagbar
  [snipmate-snippets]: https://github.com/simongmzlj/snipmate-snippets

Also Included
-------------

- A tag file for gtk-3.0 tab completion.
- French and Slovenian spell check dictionaries.

Installing
==========

First link `~/.vimrc` and `~/.gvimrc` files up as they are stored
inside the repository.

```
ln -s ~/.vim/.vimrc ~/.vimrc
ln -s ~/.vim/.gvimrc ~/.gvimrc
```

Then fetch submodules to grab pathogen and other bundles.

```
git submodules init
git submodules update
```
