" Vim color file
" Name:         diablo3
" Maintainer:   Vayn <vayn@vayn.de>
" Last Change:  2011年 08月 24日 星期三 22:44:21 CST
" Thanks To:    lilydjwg, Tomas Restrepo(author of molokai.vim), terremoto
" License:      MIT License
" Options:
"
" If you want to set long line warning, copy this in your vimrc:
"
"   let g:diablo3_longline = 1
"
"
" The default length of a line is 120, you can change it by putting
" the following in your vimrc:
"
"   let g:diablo3_len = 79
"
" Notice the length here is the length you want to set plus 1.
"
hi clear

set background=dark
if version > 580
  hi clear
  if exists("syntax_on")
    syntax reset
  endif
endif
let g:colors_name="diablo3"

" Error format when a line is longer than g:diablo3_longlen, {{{1
" default length is 120.
if exists('g:diablo3_longline') && g:diablo3_longline == 1
  if ! exists('g:diablo3_len')
    let g:diablo3_len = 121
  end
  exe 'match LongLineWarning "\%'.g:diablo3_len.'v.*"'
end

" Colors {{{1
hi Boolean         guifg=#ae81ff               gui=bold
hi Character       guifg=#e6db74
hi Number          guifg=#ae81ff
hi String          guifg=#fadc11
hi Conditional     guifg=#6d8fd9               gui=bold
hi Constant        guifg=#d4ff36               gui=bold
hi Cursor          guifg=#000000 guibg=#f8f8f0
hi Debug           guifg=#bca3a3               gui=bold
hi Define          guifg=#66d9ef
hi Delimiter       guifg=#8f8f8f
hi DiffAdd                       guibg=#13354a
hi DiffChange      guifg=#89807d guibg=#4c4745
hi DiffDelete      guifg=#960050 guibg=#1e0010
hi DiffText                      guibg=#4c4745 gui=italic,bold

hi Directory       guifg=#a6e22e               gui=bold
hi Error           guifg=#960050 guibg=#1e0010
hi ErrorMsg        guifg=#f92672 guibg=#232526 gui=bold
hi Exception       guifg=#a6e22E               gui=bold
hi Float           guifg=#ae81fF
hi FoldColumn      guifg=#465457 guibg=#000000
hi Folded          guifg=#465457 guibg=#000000
hi Function        guifg=#cbfe28
hi Identifier      guifg=#ff9900
hi Ignore          guifg=#808080 guibg=bg
hi IncSearch       guifg=#c4be89 guibg=#000000

hi Keyword         guifg=#f92672               gui=bold
hi Label           guifg=#e6db74               gui=none
hi Macro           guifg=#c4be89               gui=italic
hi SpecialKey      guifg=#d3fe36               gui=italic

hi MatchParen      guifg=#000000 guibg=#fd971f gui=bold
hi ModeMsg         guifg=#e6db74
hi MoreMsg         guifg=#e6db74
hi Operator        guifg=#6d8fd9

" complete menu
hi Pmenu           guifg=#66d9ef guibg=#000000
hi PmenuSel                      guibg=#808080
hi PmenuSbar                     guibg=#080808
hi PmenuThumb      guifg=#66d9ef

hi PreCondit       guifg=#a6e22e               gui=bold
hi PreProc         guifg=#a6e22e
hi Question        guifg=#66d9ef
hi Repeat          guifg=#f92672               gui=bold
hi Search          guifg=#fffffF guibg=#0099ff
" marks column
hi SignColumn      guifg=#a6e22e guibg=#232526
hi SpecialChar     guifg=#f92672               gui=bold
hi SpecialComment  guifg=#465457               gui=bold
hi Special         guifg=#66d9ef guibg=bg      gui=italic
hi SpecialKey      guifg=#888A85               gui=italic
if has("spell")
  hi SpellBad    guisp=#ff0000 gui=undercurl
  hi SpellCap    guisp=#7070F0 gui=undercurl
  hi SpellLocal  guisp=#70f0f0 gui=undercurl
  hi SpellRare   guisp=#ffffff gui=undercurl
endif
hi Statement       guifg=#6d8fd9               gui=bold
hi htmlStatement   guifg=#99b5d9
hi StatusLine      guifg=#455354 guibg=fg
hi StatusLineNC    guifg=#808080 guibg=#080808
hi StorageClass    guifg=#fd971f               gui=italic
hi Structure       guifg=#66d9ef
hi Tag             guifg=#f92672               gui=italic
hi Title           guifg=#ef5939
hi Todo            guifg=#ffffff guibg=bg      gui=bold

hi Typedef         guifg=#66d9ef
hi Type            guifg=#66d9ef               gui=none
hi Underlined      guifg=#808080               gui=underline

hi VertSplit       guifg=#808080 guibg=#080808 gui=bold
hi VisualNOS                     guibg=#403d3d
hi Visual                        guibg=#403d3d
hi WarningMsg      guifg=#ffffff guibg=#333333 gui=bold
hi WildMenu        guifg=#66d9ef guibg=#000000

hi Normal          guifg=#f4f4f4 guibg=#070914
hi Comment         guifg=#666666
hi CursorLine                    guibg=#293739
hi CursorColumn                  guibg=#293739
hi LineNr          guifg=#bcbcbc guibg=#151825
hi NonText         guifg=#151825 guibg=#151825

hi LongLineWarning guifg=#960050 guibg=#1e0010 gui=underline

"
" Support for 256-color terminal
"
if &t_Co > 255
  highlight Boolean         cterm=bold                    ctermfg=141
  highlight Character                                     ctermfg=185
  highlight Number                                        ctermfg=141
  highlight String                                        ctermfg=220
  highlight Conditional     cterm=bold                    ctermfg=68
  highlight Constant        cterm=bold                    ctermfg=148
  highlight Cursor                          ctermbg=231   ctermfg=16
  highlight Debug           cterm=bold                    ctermfg=250
  highlight Define                                        ctermfg=81
  highlight Delimiter                                     ctermfg=245
  highlight DiffAdd                         ctermbg=236
  highlight DiffChange                      ctermbg=238   ctermfg=244
  highlight DiffDelete                      ctermbg=232   ctermfg=89
  highlight DiffText        cterm=bold      ctermbg=238

  highlight Directory       cterm=bold                    ctermfg=112
  highlight Error                           ctermbg=232   ctermfg=89
  highlight ErrorMsg        cterm=bold      ctermbg=235   ctermfg=161
  highlight Exception       cterm=bold                    ctermfg=112
  highlight Float                                         ctermfg=141
  highlight FoldColumn                      ctermbg=16    ctermfg=239
  highlight Folded                          ctermbg=16    ctermfg=239
  highlight Function                                      ctermfg=154
  highlight Identifier                                    ctermfg=172
  highlight Ignore                                        ctermfg=244
  highlight IncSearch                       ctermbg=16    ctermfg=144

  highlight Keyword         cterm=bold                    ctermfg=161
  highlight Label           cterm=none                    ctermfg=185
  highlight Macro                                         ctermfg=144
  highlight SpecialKey                                    ctermfg=148

  highlight helpExample                                   ctermfg=178
  highlight helpOption                                    ctermfg=6
  highlight HelpHyperTextEntry                            ctermfg=2
  highlight HelpHyperTextJump                             ctermfg=33

  highlight MatchParen      cterm=bold      ctermbg=208   ctermfg=16

  highlight ModeMsg                                       ctermfg=185
  highlight MoreMsg                                       ctermfg=185
  highlight Operator        cterm=bold                    ctermfg=68

  " complete menu
  highlight Pmenu                           ctermbg=16    ctermfg=81
  highlight PmenuSel                        ctermbg=244
  highlight PmenuSbar                       ctermbg=232
  highlight PmenuThumb                                    ctermfg=81

  highlight PreCondit       cterm=bold                    ctermfg=112
  highlight PreProc                                       ctermfg=112
  highlight Question                                      ctermfg=81
  highlight Repeat          cterm=bold                    ctermfg=161
  highlight Search                          ctermbg=32    ctermfg=231
  " marks column
  highlight SignColumn                      ctermbg=235   ctermfg=112
  highlight SpecialChar     cterm=bold                    ctermfg=161
  highlight SpecialComment  cterm=bold                    ctermfg=239
  highlight Special                                       ctermfg=81
  highlight SpecialKey                                    ctermfg=245

  if has("spell")
    highlight SpellBad      cterm=bold                    ctermbg=240
    highlight SpellCap      cterm=underline
    highlight SpellLocal    cterm=underline
    highlight SpellRare     cterm=underline
  endif

  highlight Statement       cterm=bold                    ctermfg=68
  highlight htmlStatement                                 ctermfg=110
  highlight StatusLine                                    ctermfg=240
  highlight StatusLineNC                    ctermbg=232   ctermfg=244
  highlight StorageClass                                  ctermfg=208
  highlight Structure       cterm=bold                    ctermfg=81
  highlight Tag                                           ctermfg=161
  highlight Title                                         ctermfg=209
  highlight Todo            cterm=bold      ctermbg=235   ctermfg=231

  highlight Typedef                                       ctermfg=81
  highlight Type            cterm=none                    ctermfg=81
  highlight Underlined      cterm=underline               ctermfg=244

  highlight VertSplit       cterm=bold      ctermbg=232   ctermfg=244
  highlight VisualNOS                       ctermbg=237
  highlight Visual                          ctermbg=237
  highlight WarningMsg      cterm=bold      ctermbg=236   ctermfg=231
  highlight WildMenu                        ctermbg=16    ctermfg=81

  highlight Normal                          ctermbg=235   ctermfg=231
  highlight Comment                                       ctermfg=248
  highlight CursorLine      cterm=none      ctermbg=237
  highlight CursorColumn                    ctermbg=237
  highlight LineNr                          ctermbg=237   ctermfg=252
  highlight NonText                         ctermbg=237   ctermfg=235

  highlight LongLineWarning cterm=underline ctermbg=53    ctermfg=1
end
