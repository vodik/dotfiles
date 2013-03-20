" file:     ~/.vim/filetype.vim
" author:   Simon Gomizelj
" ----------------------------------------------------------------------------

" DETECTION {{{1
" ----------------------------
au BufNewFile,BufRead *.nfo setf nfo
au BufNewFile,BufRead *.txt setf txt
au BufNewFile,BufRead *.hsc setf haskell
au BufNewFile,BufRead *.chs setf haskell
au BufNewFile,BufRead *.bf  setf brainfuck
au BufNewFile,BufRead *.rl  setf ragel

au BufReadPost *.doc silent %!catdoc "%"
au BufWriteCmd *.doc setl readonly
au BufReadPost *.odt,*.odp silent %!odt2txt "%"
au BufWriteCmd *.odt setl readonly
au BufReadPost *.pdf silent %!pdftotext -nopgbrk -layout -q -eol unix "%" - | fmt -w72
au BufWriteCmd *.pdf setl readonly
au BufReadPost *.rtf silent %!unrtf --text "%"
au BufWriteCmd *.rtf setl readonly

" CODING {{{1
" ----------------------------
au FileType vim             setl sw=2 ts=2 et
au FileType c,cpp           setl cino=(0 et
au FileType html,xhtml,xml  setl sw=2 ts=2 et
au FileType hamlet          setl sw=2 ts=2 et
au FileType ruby            setl sw=2 ts=2 et
au FileType python          setl et
au FileType bash,sh,zsh     setl sw=2 ts=2 et
au FileType haskell,cabal   setl et

" TEXT {{{1
" ----------------------------
au FileType gitcommit,mail setl spell et fo+=ct
au FileType plaintex,pod   setl spell et fo+=ct
au FileType pandoc         setl spell et sw=2 ts=2 fo+=ct
