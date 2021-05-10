" Osobní nastavení, tohle by mělo být pro 'témata' v gvimu - slate vypadá
" nejlépe

colorscheme desert
" colorscheme nord " aktivuje nord-theme

set guifont=Source\ Code\ Pro\ 11

" Požadavky, aby plugin wimWiki fungoval - `nocompatible` se nastavuje vždy,
" ostatní kdoví ... Zajímavý plugin pro psaní, trochu podobný org-mode v Emacs

set nocompatible
filetype plugin on
syntax on

" Nastavení chování kurzoru - platí pouze v `gvim`, ve `vim` v terminálu platí
" nastavení terminálu
set guicursor=a:blinkon0 " vypnutí blikání kurzoru, `guifg` nastaví barvu písmen pod kurzorem, `guibg` nastavuje barvu kurzoru
:highlight Cursor guibg=#1ABC9C guifg=black

" Zvýraznění řádku s kurzorem
set cursorline
:highlight CursorLine guibg=#2c3e50

" Zvýraznění sloupce s kurzorem
" :set cursorcolumn
" :highlight CursorColumn guibg=#250162

" Nastavení pro automatické vizuální zalomování textu + zalamování pouze na
" whitespace
set wrap
set linebreak

" Nastavení číslování řádků
set number
set relativenumber

" Pomocné příkazy a přenastavení kláves pro LaTeX:
" ! Fungují v insert-módu
augroup __latex__
au!
autocmd BufRead,BufNewFile *.tex inoremap ,ch \chapter{}<Left>
autocmd BufRead,BufNewFile *.tex inoremap ,se \section{}<Left>
autocmd BufRead,BufNewFile *.tex inoremap ,sb \subsection{}<Left>
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

" Specify a directory for plugins
" - For Neovim: stdpath('data') . '/plugged'
" - Avoid using standard Vim directory names like 'plugin'

" Make sure you use single quotes

" Shorthand notation; fetches https://github.com/junegunn/vim-easy-align
" Plug 'junegunn/vim-easy-align'

" Any valid git URL is allowed
" Plug 'https://github.com/junegunn/vim-github-dashboard.git'

call plug#begin()

" Airline plugin
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" Rainbow Parenthesses plugin
Plug 'luochen1990/rainbow'
let g:rainbow_active = 1

" vim-signature plugin : Použití vlastních záložek
Plug 'https://github.com/kshenoy/vim-signature'

" On-demand loading
Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' }

" Distraction-free writing ve vimu
" `:Goyo` zapíná, `:Goyo!` vypíná
Plug 'junegunn/goyo.vim', { 'on': 'Goyo' }

" Zvýražnování textu pomocí změny jasu fontu; pro některá nastavení může být nutná ruční úprava, návod na GitHubu
" `:Limelight` zapne limelight, volitelně lze připsat číslo [0.0 - 1.0]; `:Limelight!` vypne limelight
Plug 'junegunn/limelight.vim', { 'on': 'Limelight' }

" Cílení na text za objektem - vyžaduje úpravu nastavení pro definování objektů
Plug 'junegunn/vim-after-object'
autocmd VimEnter * call after_object#enable('=', ':', '-', '#', ' ') " za těmito objekty bude vybírat text

" Dim inactive windows, `:DimInactiveWindowOff` vypne funkci pro dané okno, `:DimInactiveWindowOn` funkci zapne. Více příkazů je na stránce projektu
Plug 'https://github.com/blueyed/vim-diminactive', { 'on': 'DimInactiveWindowOn' }
" Když chci plugin používat, stačí použít jeho hlavní funkci na nějaké okno - v tu chvíli se načte

" Zvýrazňování syntaxe markdownu a textu odrážek, je nutné `: set filetype=journal:`
Plug 'junegunn/vim-journal'

" <TAB> completion zápisu vyhledávacích příkazů
Plug 'https://github.com/vim-scripts/SearchComplete'

" Plugin NERD Commenter
Plug 'https://github.com/scrooloose/nerdcommenter'

" Indexovaná reference lua přímo ve vimu
Plug 'https://github.com/vim-scripts/luarefvim'

" Matchit - verze pro plugin-managery
Plug 'https://github.com/adelarsq/vim-matchit'

" Surround 
Plug 'https://github.com/tpope/vim-surround'

" Vkládání code snippets, SnipMate, změnil hlavní repositář
Plug 'MarcWeber/vim-addon-mw-utils'
Plug 'tomtom/tlib_vim'
Plug 'https://github.com/garbas/vim-snipmate'

let g:snipMate = { 'snippet_version': 1 }

" Undo historie
Plug 'https://github.com/sjl/gundo.vim'
Plug 'https://github.com/mbbill/undotree'

" Více kurzorů najednou, help je pomocí `:help visual-multi`
Plug 'mg979/vim-visual-multi', {'branch': 'master'}

" Souštění REPLu přes Screen nebo tmux z textu ve vimu
Plug 'https://github.com/jpalardy/vim-slime'

" Zajímavý plugin pro psaní, trochu podobný org-mode v Emacs
Plug 'vimwiki/vimwiki'

" Zvýrazňovač barev (? v CSS?) - podporuje velké množství syntaxe
Plug 'https://github.com/ap/vim-css-color'

" File manager ve vimu
Plug 'vifm/vifm.vim'

" Vyhledávání souborů pomocí `fzf`
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }

" Podpora psaní LaTeXu ve `vim`u
Plug 'lervag/vimtex'

let g:tex_flavor = 'latex'

" Podpora pro psaní v `pandoc`u a jeho syntaxe
Plug 'vim-pandoc/vim-pandoc'
Plug 'vim-pandoc/vim-pandoc-syntax'

" Práce s a automatické formátování textových tabulek
Plug 'dhruvasagar/vim-table-mode'

" Plugin pro seřazování řádků jiným než číselným způsobem; lze také řadit
" podle jiného než prvního řádku
Plug 'https://github.com/vim-scripts/vis'

" Plugin pro transformaci seznamu v dlouhou větu oddělenou čárkami a obráceně
Plug 'https://github.com/soulston/vim-listtrans'

" Plugin pro Nord theme
Plug 'arcticicestudio/nord-vim'

" Plugin pro fancy ikony
Plug 'ryanoasis/vim-devicons'

call plug#end()
