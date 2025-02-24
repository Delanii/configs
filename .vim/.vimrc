" Nastavení fontu. Nastavení témat je až na konci souboru, aby téma vim našel.

set guifont=Fira\ Code\ 12
set background=dark

" Požadavky, aby plugin wimWiki fungoval - `nocompatible` se nastavuje vždy,
" ostatní kdoví ... Zajímavý plugin pro psaní, trochu podobný org-mode v Emacs

set nocompatible
filetype plugin on
syntax on

" Nastavení chování kurzoru - platí pouze v `gvim`, ve `vim` v terminálu platí
" nastavení terminálu
set guicursor=a:blinkon0 " vypnutí blikání kurzoru, `guifg` nastaví barvu písmen pod kurzorem, `guibg` nastavuje barvu kurzoru
highlight Cursor guibg=#1ABC9C guifg=black

" Zvýraznění řádku s kurzorem
set cursorline
set cursorlineopt=both
highlight CursorLine guibg=#2c3e50

" Zvýraznění sloupce s kurzorem
" set cursorcolumn
" highlight CursorColumn guibg=#250162

" Nastavení pro automatické vizuální zalomování textu + zalamování pouze na
" whitespace
set wrap
set linebreak

" Nastavení číslování řádků
set number
set relativenumber

" Zvýraznění tabulátorů, trailing whitespace a nezlomných mezer -- tyhle věci dělají problém i v kódování
exec "set listchars=tab:\uBB\uBB,trail:\uB7,nbsp:~"
set list

set ignorecase
set clipboard=unnamed

" Settings for `git vimdiff` highlighting
" odkaz: https://stackoverflow.com/questions/2019281/load-different-colorscheme-when-using-vimdiff
"
highlight DiffAdd    cterm=bold ctermfg=10 ctermbg=17 gui=none guifg=bg guibg=Red
highlight DiffDelete cterm=bold ctermfg=10 ctermbg=17 gui=none guifg=bg guibg=Red
highlight DiffChange cterm=bold ctermfg=10 ctermbg=17 gui=none guifg=bg guibg=Red
highlight DiffText   cterm=bold ctermfg=10 ctermbg=88 gui=none guifg=bg guibg=Red

" cterm - sets the style
" ctermfg - set the text color
" ctermbg - set the highlighting
" DiffAdd - line was added
" DiffDelete - line was removed
" DiffChange - part of the line was changed (highlights the whole line)
" DiffText - the exact part of the line that changed

set conceallevel=0

" Specify a directory for plugins
" - For Neovim: stdpath('data') . '/plugged'
" - Avoid using standard Vim directory names like 'plugin'

" Make sure you use single quotes

" Shorthand notation; fetches https://github.com/junegunn/vim-easy-align
" Plug 'junegunn/vim-easy-align'

" Any valid git URL is allowed
" Plug 'https://github.com/junegunn/vim-github-dashboard.git'

call plug#begin()

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
" Theming
"
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""ů

" Airline plugin
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" Vim one theme, seems to look better than 'desert' and 'nord' themes
Plug 'https://github.com/joshdick/onedark.vim'

" Plugin pro fancy ikony
Plug 'ryanoasis/vim-devicons'

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
" Syntax highlighting
"
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Syntax highlighting of all possible languages (almost)
Plug 'sheerun/vim-polyglot'

""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
" General highlighting
"
""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Rainbow Parenthesses plugin
Plug 'luochen1990/rainbow'
let g:rainbow_active = 1

" vim-signature plugin : Použití vlastních záložek
Plug 'https://github.com/kshenoy/vim-signature'

" Zvýrazňovač barev (? v CSS?) - podporuje velké množství syntaxe
Plug 'https://github.com/ap/vim-css-color'

"""""""""""""""""""""""""""""""""""""""""""""
"
" Keybindings
"
""""""""""""""""""""""""""""""""""""""""""""

" Which-key for vim
Plug 'liuchengxu/vim-which-key'


" Remaps

let mapleader = ","

" Windows

" Remaps C-w to create new windows and move cursor in them to the <Leader>w key sequence
nnoremap <Leader>w <C-w>
nnoremap <C-w> <Nop>

" Tags

" Zellij multiplexer uses C-t to switch the multiplexer tags. Vim user C-t to navigate between tags. Lets remap that to avoid shadowing.
nnoremap <Leader>t <C-t>
nnoremap <C-t> <Nop>

"""""""""""""""""""""""""""""""""""""""""""
"
" Writing code
"
"""""""""""""""""""""""""""""""""""""""""""

" Více kurzorů najednou, help je pomocí `:help visual-multi`
Plug 'mg979/vim-visual-multi', {'branch': 'master'}

" Git integration
Plug 'https://github.com/tpope/vim-fugitive'

" Adding comments the classic way
Plug 'preservim/nerdcommenter'

" Souštění REPLu přes Screen nebo tmux z textu ve vimu
Plug 'https://github.com/jpalardy/vim-slime'
" Obdobný plugin - pro scratchpad, kde může běžet více REPLů
Plug 'metakirby5/codi.vim'

" Indexovaná reference lua přímo ve vimu
Plug 'https://github.com/vim-scripts/luarefvim'

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
" General writing
"
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Surround 
Plug 'https://github.com/tpope/vim-surround'

" Matchit - verze pro plugin-managery
Plug 'https://github.com/adelarsq/vim-matchit'

" Cílení na text za objektem - vyžaduje úpravu nastavení pro definování objektů
Plug 'junegunn/vim-after-object'
autocmd VimEnter * call after_object#enable('=', ':', '-', '#', ' ') " za těmito objekty bude vybírat text

" Undo historie
Plug 'https://github.com/sjl/gundo.vim'
Plug 'https://github.com/mbbill/undotree'

" <TAB> completion zápisu vyhledávacích příkazů
Plug 'https://github.com/vim-scripts/SearchComplete'

"
" LaTeX
"

" Pomocné příkazy a přenastavení kláves pro LaTeX:
" ! Fungují v insert-módu
augroup __latex__
au!
autocmd BufRead,BufNewFile *.tex inoremap ,ch \chapter{}<Left>
autocmd BufRead,BufNewFile *.tex inoremap ,se \section{}<Left>
autocmd BufRead,BufNewFile *.tex inoremap ,sb \rubsection{}<Left>
autocmd BufRead,BufNewFile *.tex inoremap ,sbb \subsubsection{}<Left>
autocmd BufRead,BufNewFile *.tex inoremap ,em \emph{}<Left>
autocmd BufRead,BufNewFile *.tex inoremap ,tt \texttt{}<Left>
autocmd BufRead,BufNewFile *.tex inoremap ,bf \textbf{}<Left>
autocmd BufRead,BufNewFile *.tex inoremap ,uv \enquote{}<Left>
autocmd BufRead,BufNewFile *.tex inoremap ,tab \begin{tabular}{}<CR><CR>\end{tabular}<Up>
autocmd BufRead,BufNewFile *.tex inoremap ,tbl \begin{table}[htb]<CR><CR>\caption{}<CR>\label{}<CR>\end{table}<C-O>3k
autocmd BufRead,BufNewFile *.tex inoremap ,fig \begin{figure}[htb]<CR><CR>\caption{}<CR>\label{}<CR>\end{figure}<C-O>3k
autocmd BufRead,BufNewFile *.tex inoremap ,itz \begin{itemize}<CR>\item <CR>\end{itemize}<CR><Up><Up>
autocmd BufRead,BufNewFile *.tex inoremap ,enu \begin{enumerate}<CR>\item <CR>\end{enumerate}<CR><Up><Up>
autocmd BufRead,BufNewFile *.tex inoremap ,it \item<Space>
autocmd BufRead,BufNewFile *.tex inoremap ,in \index{!}<Left><Left>
autocmd BufRead,BufNewFile *.tex inoremap ,gl \gls{!}<Left><Left>
autocmd BufRead,BufNewFile *.tex inoremap ,re \ref{}<Left>
autocmd BufRead,BufNewFile *.tex inoremap ,cre \cref{}<Left>
autocmd BufRead,BufNewFile *.tex inoremap ,vre \vref{}<Left><Left>
autocmd BufRead,BufNewFile *.tex inoremap ,pre \pageref{}<Left><Left>
autocmd BufRead,BufNewFile *.tex inoremap ,in \index{!}<Left><Left>
augroup END

" Podpora psaní LaTeXu ve `vim`u
Plug 'lervag/vimtex'

let g:tex_flavor = 'latex'

"
" Pandoc
"

" Podpora pro psaní v `pandoc`u a jeho syntaxe
Plug 'vim-pandoc/vim-pandoc'
Plug 'vim-pandoc/vim-pandoc-syntax'

"
" More general editing plugins
"

" Práce s a automatické formátování textových tabulek
Plug 'dhruvasagar/vim-table-mode'

" Z prezentace Damiana Conwaye
" Plugin pro manipulaci s celými řádky podle označených visual blocků
Plug 'https://github.com/vim-scripts/vis'

" Splitjoin - splits or joins text along delimiters
Plug 'https://github.com/AndrewRadev/splitjoin.vim'

"!! spellchecking -- check `:help mkspell`!!

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
" Text navigation
"
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Easymotion - jumping around the file with searching, same as doom emacs searching
Plug 'easymotion/vim-easymotion'

" Další motion pluginy
Plug 'tpope/vim-repeat' 
" Required by sneak
Plug 'justinmk/vim-sneak'
Plug 'rhysd/clever-f.vim'

" Zvýražnování textu pomocí změny jasu fontu; pro některá nastavení může být nutná ruční úprava, návod na GitHubu
" `:Limelight` zapne limelight, volitelně lze připsat číslo [0.0 - 1.0]; `:Limelight!` vypne limelight
Plug 'junegunn/limelight.vim', { 'on': 'Limelight' }

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
" File management and file searching
"
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" File manager ve vimu
Plug 'vifm/vifm.vim'

" Vyhledávání souborů pomocí `fzf`
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }

" On-demand loading
Plug 'preservim/nerdtree', { 'on': 'NERDTreeToggle' }

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
" Vim windows management
"
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

call plug#end()

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
" More theme settings
"
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" For some reason, this has to be after `call plug#end()`; find out why ... !

if (has("autocmd"))
  augroup colorextend
    autocmd!
    " Override the `Comment` foreground color in 256-color mode (in terminal)
    autocmd ColorScheme * call onedark#extend_highlight("Comment", { "fg": { "cterm": 64 } })
  augroup END
endif

" colorscheme desert
" colorscheme nord " aktivuje nord-theme
colorscheme onedark
let g:airline_theme='onedark' " onedark theme also for airline

" Colorscheme overrides
"
highlight LineNr ctermfg=99
