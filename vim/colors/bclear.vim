" Vim colorscheme
" Name:         bclear
" Maintainer:   Ricky Cintron 'borosai' <borosai at gmail dot com>
" Last Change:  2010-10-17

hi clear
set background=light
if exists("syntax_on")
  syntax reset
endif
let g:colors_name = "bclear"

" GUI SETTINGS {{{1
" -----------------------------

hi SpecialKey   guifg=#b9b9b9
hi NonText      guifg=#969696   guibg=#f0f0f0   gui=NONE
hi Directory    guifg=#78681a
hi ErrorMsg     guifg=#ffffff   guibg=#a01010
hi IncSearch    guifg=#ffffff   guibg=#ff8000   gui=NONE
hi Search       guifg=#000000   guibg=#ffd073
hi MoreMsg      guifg=#ffffff   guibg=#3c960f   gui=NONE
hi ModeMsg      guifg=#323232                   gui=NONE
hi LineNr       guifg=#969696   guibg=#f0f0f0
hi Question     guifg=#000000   guibg=#ffde37   gui=NONE
hi StatusLine   guifg=#ffffff   guibg=#323232   gui=NONE
hi StatusLineNC guifg=#f0f0f0   guibg=#646464   gui=NONE
hi VertSplit    guifg=#f0f0f0   guibg=#646464   gui=NONE
hi Title        guifg=#dc6816                   gui=NONE
hi Visual       guifg=#ffffff   guibg=#1994d1
hi VisualNOS    guifg=#000000   guibg=#1994d1   gui=NONE
hi WarningMsg   guifg=#c8c8c8   guibg=#a01010
hi WildMenu     guifg=#ffffff   guibg=#1994d1
hi Folded       guifg=#969696   guibg=#f0f0f0
hi FoldColumn   guifg=#969696   guibg=#f0f0f0
hi DiffAdd                      guibg=#deffcd
hi DiffChange                   guibg=#dad7ff
hi DiffDelete   guifg=#c8c8c8   guibg=#ffffff   gui=NONE
hi DiffText     guifg=#ffffff   guibg=#767396   gui=NONE
hi SignColumn   guifg=#969696   guibg=#f0f0f0
hi Conceal      guifg=#969696   guibg=#f0f0f0
hi SpellBad     guifg=#000000   guibg=#fff5c3   guisp=#f01818   gui=undercurl
hi SpellCap     guifg=#000000   guibg=#fff5c3   guisp=#14b9c8   gui=undercurl
hi SpellRare    guifg=#000000   guibg=#fff5c3   guisp=#4cbe13   gui=undercurl
hi SpellLocal   guifg=#000000   guibg=#fff5c3   guisp=#000000   gui=undercurl
hi Pmenu        guifg=#ffffff   guibg=#323232
hi PmenuSel     guifg=#ffffff   guibg=#1994d1
hi PmenuSbar    guifg=#323232   guibg=#323232
hi PmenuThumb   guifg=#646464   guibg=#646464   gui=NONE
hi TabLine      guifg=#f0f0f0   guibg=#646464   gui=NONE
hi TabLineSel   guifg=#ffffff   guibg=#323232   gui=NONE
hi TabLineFill  guifg=#646464   guibg=#646464   gui=NONE
hi CursorColumn                 guibg=#e1f5ff
hi CursorLine                   guibg=#e1f5ff   gui=NONE
hi ColorColumn                  guibg=#b8ddf0
hi Cursor       guifg=#ffffff   guibg=#323232
hi lCursor      guifg=#ffffff   guibg=#004364
hi MatchParen   guifg=#f00078   guibg=#ffffff
hi Normal       guifg=#323232   guibg=#ffffff
hi Comment      guifg=#969696                   gui=italic
hi Constant     guifg=#1094a0
hi Special      guifg=#dc6816
hi Identifier   guifg=#3c960f
hi Function     guifg=#f83b3b
hi Statement    guifg=#3b6ac8                   gui=NONE
hi PreProc      guifg=#294a8c
hi Type         guifg=#a00050                   gui=NONE
hi Underlined   guifg=#323232                   gui=underline
hi Ignore       guifg=#c8c8c8
hi Error        guifg=#f00078   guibg=#000000
hi Todo         guifg=#c81414   guibg=#ffffff

hi link         String          Constant

" FILE TYPE SETTINGS {{{1
" -----------------------------

" pandoc
hi pandocSetexHeader guifg=#a00050 gui=bold
