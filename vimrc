set nocompatible                    " this has to be first

syntax on                           " turn on syntax highlighting

set hidden                          " allow switching away from modified buffers
set backspace=indent,eol,start      " make backspace work over indents and start/end of line
set showcmd                         " display incomplete commands
"set title                           " update the terminal's title with Vim's info
set wildmenu                        " show all matches when using tab completion
set notagrelative                   " don't assume tag paths are relative to tag file directory
set ruler                           " show the cursor position at all times

set noautoindent
set cindent
set smartindent

set shiftwidth=4
set tabstop=4
set softtabstop=4
set expandtab
set shiftround                      " always indent/outdent to the nearest tabstop

set hlsearch                        " highlight search matches
set incsearch                       " show search matches as you type
set ignorecase                      " perform case insensitive searches ...
set smartcase                       " ... unless the search includes an uppercase letter

if has("autocmd")
    filetype plugin indent on       " indent source code based on file type

    " Automatically wrap text in SVN commit messages
    autocmd FileType svn setlocal textwidth=72
endif

if has("gui_running")
    set guifont=DejaVu\ Sans\ Mono\ 11
    set guioptions+=b               " show the bottom (horizontal) scrollbar
    set guioptions-=T               " disable the toolbar
    colorscheme molokai 
else
    "colorscheme impact
endif

" Show hidden characters (tabs and spaces) when you press the leader key
" followed by the 'l' key.
nmap <leader>l :set list!<CR>

" Show syntax highlighting groups for word under cursor
nmap <C-S-P> :call <SID>SynStack()<CR>
function! <SID>SynStack()
  if !exists("*synstack")
    return
  endif
  echo map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")')
endfunc

