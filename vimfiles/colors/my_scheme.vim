"  My Custom Color Scheme
"  Creator: Tom Small III
"  Last Change: June 22, 2004
"  Version: 0.1
"
"  Description:
"  This color scheme has a desert-ish look, mimicing one of the colorschemes
"  that comes with CodeWright.

set background=light
hi clear
if exists("syntax_on")
   syntax reset
endif

let colors_name = "my_scheme"

hi Normal                     guifg=#000000  guibg=#D5CCBB

" Search
hi IncSearch      gui=NONE    guifg=#000000  guibg=#CD8162
"hi Search         gui=NONE    guifg=#000000  guibg=#008080
hi Search         gui=NONE    guifg=#000000  guibg=#FFFF00

" Messages
hi ErrorMsg       gui=BOLD    guifg=#000080  guibg=#4040ff
hi WarningMsg     gui=BOLD    guifg=#000080  guibg=#4040ff
hi ModeMsg        gui=NONE    guifg=#d06000  guibg=NONE
hi MoreMsg        gui=NONE    guifg=#0090a0  guibg=NONE
hi Question       gui=NONE    guifg=#8000ff  guibg=NONE

" Split area
hi StatusLine     gui=BOLD    guifg=#F8F8F8  guibg=#006E96
hi StatusLineNC   gui=BOLD    guifg=#C0B0A0  guibg=#006E96
hi VertSplit      gui=NONE    guifg=#F8F8F8  guibg=#006E96
hi WildMenu       gui=BOLD    guifg=#F8F8F8  guibg=#ff3030

" Diff
hi DiffText       gui=NONE    guifg=#2850a0  guibg=#C0D0F0
hi DiffChange     gui=NONE    guifg=#208040  guibg=#C0F0D0
hi DiffDelete     gui=NONE    guifg=#ff2020  guibg=#eaf2b0
hi DiffAdd        gui=NONE    guifg=#ff2020  guibg=#eaf2b0

" Cursor
hi Cursor         gui=NONE    guifg=#ffffff  guibg=#000000
hi lCursor        gui=NONE    guifg=#ffffff  guibg=#8040ff
hi CursorIM       gui=NONE    guifg=#ffffff  guibg=#8040ff

" Fold
hi Folded         gui=NONE    guifg=fg       guibg=NONE
hi FoldColumn     gui=NONE    guifg=fg       guibg=NONE

" Other
hi Directory      gui=NONE    guifg=#008080  guibg=NONE
hi LineNr         gui=NONE    guifg=#000000  guibg=NONE
hi NonText        gui=BOLD    guifg=#a05040  guibg=NONE
hi SpecialKey     gui=NONE    guifg=#0080ff  guibg=NONE
hi Title          gui=BOLD    guifg=fg       guibg=NONE
hi Visual         gui=NONE    guifg=NONE     guibg=#008080

" Syntax group
hi Comment        gui=ITALIC  guifg=#8B5A2B  guibg=NONE
hi Constant       gui=NONE    guifg=#00884c  guibg=NONE
hi Error          gui=BOLD    guifg=#f8f8f8  guibg=#4040ff
hi Identifier     gui=NONE    guifg=#b07800  guibg=NONE
hi Ignore         gui=NONE    guifg=bg       guibg=NONE
hi PreProc        gui=NONE    guifg=#0090a0  guibg=NONE
hi Special        gui=NONE    guifg=#008080  guibg=NONE
hi Statement      gui=BOLD    guifg=#000080  guibg=NONE
hi Todo           gui=BOLD,UNDERLINE guifg=#0080f0 guibg=NONE
hi Type           gui=BOLD    guifg=#b06c58  guibg=NONE
hi Underlined     gui=UNDERLINE guifg=blue   guibg=NONE
hi Function       gui=BOLD    guifg=#000000  guibg=NONE
