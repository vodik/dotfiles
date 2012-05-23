" file:     ~/.vim/filetype.vim
" author:   Simon Gomizelj
" ----------------------------------------------------------------------------

" DETECTION {{{1
" ----------------------------
au BufNewFile,BufRead *.nfo setf nfo
au BufNewFile,BufRead *.txt setf txt
au BufNewFile,BufRead *.md  setf pandoc
au BufNewFile,BufRead *.hsc setf haskell
au BufNewFile,BufRead *.chs setf haskell
au BufNewFile,BufRead *.bf  setf brainfuck
au BufNewFile,BufRead /etc/nginx/conf/* setf nginx

au BufReadPost *.doc silent %!catdoc "%"
au BufWriteCmd *.doc setl readonly
au BufReadPost *.odt,*.odp silent %!odt2txt "%"
au BufWriteCmd *.odt setl readonly
au BufReadPost *.pdf silent %!pdftotext -nopgbrk -layout -q -eol unix "%" - | fmt -w78
au BufWriteCmd *.pdf setl readonly
au BufReadPost *.rtf silent %!unrtf --text "%"
au BufWriteCmd *.rtf setl readonly

" CODING {{{1
" ----------------------------
" formating {{{2
au FileType vim             setl sw=2 ts=2 et
au FileType html,xhtml,xml  setl sw=2 ts=2 et
au FileType ruby            setl sw=2 ts=2 et
au FileType python          setl et
au FileType bash,sh,zsh     setl sw=2 ts=2 et
au FileType haskell,cabal   setl et

" listchars and format options override
au FileType vim             setl lcs+=tab:▸\ ,trail:- fo=cqrn1
au FileType c,cpp           setl lcs+=tab:▸\ ,trail:- fo=cqrn1
au FileType java            setl lcs+=tab:▸\ ,trail:- fo=cqrn1
au FileType html,xhtml,xml  setl lcs+=tab:▸\ ,trail:- fo=cqrn1
au FileType ruby            setl lcs+=tab:▸\ ,trail:- fo=cqrn1
au FileType python          setl lcs+=tab:▸\ ,trail:- fo=cqrn1
au FileType bash,sh,zsh     setl lcs+=tab:▸\ ,trail:- fo=cqrn1
au FileType haskell         setl lcs+=tab:▸\ ,trail:- fo=cqrn1

au FileType c,cpp           setl tags+=~/.vim/tags/gtk3
au FileType c,cpp           setl tags+=~/.vim/tags/cairo
au FileType c,cpp           setl tags+=~/.vim/tags/pango

" TEXT {{{1
" ----------------------------
au FileType plaintex        setl spell tw=72 fo+=cqt wm=0
au FileType gitcommit       setl spell tw=72 fo+=cqt wm=0
au FileType pandoc,markdown setl spell tw=72 fo+=cqt wm=0
