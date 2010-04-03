" When started as "evim", evim.vim will already have done these settings.
if v:progname =~? "evim"
  finish
endif

" Use Vim settings, rather then Vi settings (much better!).
" This must be first, because it changes other options as a side effect.
set nocompatible

" allow backspacing over everything in insert mode
set backspace=indent,eol,start

set history=50    " keep 50 lines of command line history
set ruler		      " show the cursor position all the time
set showcmd		    " display incomplete commands
set incsearch		  " do incremental searching

" Don't use Ex mode, use Q for formatting
map Q gq

" Switch syntax highlighting on, when the terminal has colors
" Also switch on highlighting the last used search pattern.
if &t_Co > 2 || has("gui_running")
  syntax on
  set hlsearch
endif

" Only do this part when compiled with support for autocommands.
if has("autocmd")

  " Enable file type detection.
  " Use the default filetype settings, so that mail gets 'tw' set to 72,
  " 'cindent' is on in C files, etc.
  " Also load indent files, to automatically do language-dependent indenting.
  filetype plugin indent on

  " Put these in an autocmd group, so that we can delete them easily.
  augroup vimrcEx
  au!

  " For all text files set 'textwidth' to 70 characters.
  autocmd FileType text setlocal textwidth=72

  " When editing a file, always jump to the last known cursor position.
  " Don't do it when the position is invalid or when inside an event handler
  " (happens when dropping a file on gvim).
  autocmd BufReadPost *
    \ if line("'\"") > 0 && line("'\"") <= line("$") |
    \   exe "normal g`\"" |
    \ endif

  augroup END

else

  set autoindent		" always set autoindenting on

endif " has("autocmd")


" Tab settings
set shiftwidth=4
set tabstop=4
set softtabstop=4
set expandtab 

" Set file format to DOS format
"set ff=dos

" Enable the mouse
set mouse=a

" Set quickfix errorformat
"set efm=\"%f\"\\,\ line\ %l:\ error\ (%n):\ %m

" Set F4 key to call the Comment() function (from the
" comment.vim plugin)
"map <F4> :call Comment()<CR>

" Set Shift+F4 key to call the Uncomment() function
" (from the comment.vim plugin)
"map <S-F4> :call Uncomment()<CR>

" Set F5 key to toggle the Window Manager plugin
"nn <silent> <F5> :Tlist<CR>
nn <F5> :WMToggle<CR>

" Set Shift+F5 key to refresh the tag list
nn <silent> <S-F5> :TlistSync<CR>

" Set F6 key to toggle line wrapping
nn <F6> :setl wrap!<Bar>echon "wrap="&wrap<CR>

" Set F7 key to toggle search highlighting
"nn <F7> :setl hlsearch!<Bar>echon "highlight="&hlsearch<CR>

" Set F8 key to read from "rerrs" file in current directory
"nn <F8> :cfile rerrs<CR>

" Set F9 key to setup a recursive grep operation
nn <F9> :grep /s /r /c:"" *.c *.cpp *.h *.hpp <C-Left><C-Left><C-Left><C-Left><Left><Left>

" Set Shift+F9 key to setup a local grep operation
nn <S-F9> :grep "" *.c *.cpp *.h *.hpp <C-Left><C-Left><C-Left><C-Left><Left><Left>

" Set Ctrl+F9 key to setup a recursive, non-regular expression grep operation
nn <C-F9> :grep /s /l /c:"" *.c *.cpp *.h *.hpp <C-Left><C-Left><C-Left><C-Left><Left><Left>

" Map the <Space> key to insert a single space when in command mode
nn <Space> i<Space><Esc>

" Allow hidden buffers
set hidden

" Turn off always equal windows
set noea

" Set the session options
set sessionoptions=buffers,curdir,folds,help,options,winsize,resize

"----- GUI Options -----
if has("gui_running")
    set gfn=Consolas:h11:cANSI
    set lines=40
    set columns=100
    set guioptions+=b
    colorscheme my_scheme 
    "set browsedir="/cygdrive/c/ppc"
    "set tags=C:\ppc\tags
else
    "set tags=/cygdrive/c/ppc/tags
    "set runtimepath+=/cygdrive/c/vim/vimfiles
    "colorscheme seashell
endif

" Variables for TagList plugin
let Tlist_Ctags_Cmd = "c:/cygwin/bin/ctags.exe"
let Tlist_Inc_Winwidth = 0
let Tlist_File_Fold_Auto_Close = 1

" Variables for WinManager plugin
let winManagerWindowLayout = 'FileExplorer,TagList|BufExplorer'
map <c-w><c-b> :BottomExplorerWindow<cr>
map <c-w><c-f> :FirstExplorerWindow<cr>

" Custom Abbreviations
abbreviate dbg DbgOut( 1, "***
