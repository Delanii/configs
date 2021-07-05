" Has to be set before plugins so nvim-colorizer may work
set termguicolors                " fg and bg highlighting requires compatible terminal colors
syntax on

set cursorline
" Nastavení pro automatické vizuální zalomování textu + zalamování pouze na
" whitespace
set wrap
set linebreak

set smartcase " makes searching with no upper-case letters to ignore case and with upper-case letters makes search case sensitive

" General Settings
set number                      " set line numbers
set relativenumber
set updatetime=100              " set update time for gitgutter update
set noswapfile                  " no swap

" tabs and spaces
set expandtab                   " Use spaces instead of tabs.
set smarttab                    " Uses shiftwidth and tabstap to insert blanks when <Tab>
set shiftwidth=2                " One tab == four spaces.
set tabstop=2                   " One tab == four spaces.<Paste>

source $HOME/.config/nvim/vim-plug/plugins.vim

call plug#begin(stdpath('data') . '/plugged')

"""""""""""""""""""""""""""""""""""""""""""""""""""""
"
" Theming
"
""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Airline plugin
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" Tokyo night theme -- doesnt seem to work with nvim < 0.5
Plug 'folke/tokyonight.nvim'

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
" General highlighting
"
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Rainbow Parenthesses plugin
Plug 'luochen1990/rainbow'
let g:rainbow_active = 1

" vim-signature plugin : Použití vlastních záložek
Plug 'https://github.com/kshenoy/vim-signature'

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
" File management
"
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" On-demand loading
Plug 'preservim/nerdtree', { 'on': 'NERDTreeToggle' }


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
" Text navigation
"
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Zvýražnování textu pomocí změny jasu fontu; pro některá nastavení může být nutná ruční úprava, návod na GitHubu
" `:Limelight` zapne limelight, volitelně lze připsat číslo [0.0 - 1.0]; `:Limelight!` vypne limelight
Plug 'junegunn/limelight.vim', { 'on': 'Limelight' }

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
" Code writing
"
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""


" Cílení na text za objektem - vyžaduje úpravu nastavení pro definování objektů
Plug 'junegunn/vim-after-object'
autocmd VimEnter * call after_object#enable('=', ':', '-', '#', ' ') " za těmito objekty bude vybírat text

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
" General writing
"
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Distraction-free writing ve vimu
" `:Goyo` zapíná, `:Goyo!` vypíná
Plug 'junegunn/goyo.vim', { 'on': 'Goyo' }

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
" Window management
"
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

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

" Easymotion - jumping around the file with searching, same as doom emacs searching
Plug 'easymotion/vim-easymotion'

" Další motion pluginy
Plug 'tpope/vim-repeat' " Required by sneak
Plug 'justinmk/vim-sneak'

Plug 'rhysd/clever-f.vim'

" Pro Neovim, možná bude vyžadovat nvim 0.5
Plug 'phaazon/hop.nvim'

" Surround 
Plug 'https://github.com/tpope/vim-surround'

" Splitjoin - splits or joins text along delimiters
Plug 'https://github.com/AndrewRadev/splitjoin.vim'

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

"
" Git integration
"

" Git integration for neovim -- classic, that is not using lua, seems to be superior, at least for now
Plug 'https://github.com/tpope/vim-fugitive'

" Magit pro neovim
" Plug 'nvim-lua/plenary.nvim'
" Plug 'TimUntersberger/neogit' " Does work only with nvim v. 0.5

" Git signs in gutter
Plug 'nvim-lua/plenary.nvim'
Plug 'lewis6991/gitsigns.nvim'

" Vyhledávací engine
Plug 'nvim-lua/popup.nvim'
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-telescope/telescope.nvim'

" Tree fileviewer
Plug 'kyazdani42/nvim-web-devicons'
" for file icons
Plug 'kyazdani42/nvim-tree.lua'

let g:nvim_tree_quit_on_open = 1 "0 by default, closes the tree when you open a file

" which-key for neovim
Plug 'folke/which-key.nvim'

" Completion system
Plug 'https://github.com/hrsh7th/nvim-compe'

" Souštění REPLu přes Screen nebo tmux z textu ve vimu
Plug 'https://github.com/jpalardy/vim-slime'

" Spouštění dalších terminálů přímo v neovimu
Plug 'kassio/neoterm' 

" Obdobný plugin - pro scratchpad, kde může běžet více REPLů
Plug 'metakirby5/codi.vim'

" Installation of lsp servers
Plug 'neovim/nvim-lspconfig'
Plug 'kabouzeid/nvim-lspinstall'

" Zajímavý plugin pro psaní, trochu podobný org-mode v Emacs
Plug 'vimwiki/vimwiki'

" Z prezentace Damiana Conwaye
" Plugin pro manipulaci s celými řádky podle označených visual blocků
Plug 'https://github.com/vim-scripts/vis'

" Zvýrazňovač barev
Plug 'norcalli/nvim-colorizer.lua'

" File manager ve vimu
Plug 'vifm/vifm.vim'

" Vyhledávání souborů pomocí `fzf`
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }

" Tree-sitter pro nvim
Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}

" Writing comments
Plug 'b3nj5m1n/kommentary'

" Show indention marks
Plug 'lukas-reineke/indent-blankline.nvim'

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

Plug 'rakr/vim-one'                  " vim-one color theme
Plug 'scrooloose/nerdtree'           " side bar file tree
" Plug 'itchyny/lightline.vim'         " minmal status bar
Plug 'tpope/vim-fugitive'            " allows git commands in vim session
Plug 'airblade/vim-gitgutter'        " shows git changes in gutter
Plug 'easymotion/vim-easymotion'     " go to any word quickly '\\w', '\\e', '\\b'
Plug 'KKPMW/vim-sendtowindow'        " send commands to REPL
Plug 'yuttie/comfortable-motion.vim' " scrolling 'C-d' or 'C-u'
Plug 'ncm2/ncm2'                     " completion [dep]: nvim-0.2.2, nvim-yarp, python3
Plug 'roxma/nvim-yarp'               " remote plugin framework required for ncm2
Plug 'ncm2/ncm2-bufword'             " complete words in buffer
Plug 'ncm2/ncm2-path'                " complete paths
Plug 'ncm2/ncm2-jedi'                " Python completion
Plug 'gaalcaras/ncm-R'               " R completion [dep]: ncm2, Nvim-R
Plug 'jalvesaq/Nvim-R'               " required for ncm-R
Plug 'dense-analysis/ale'            " linting [dep]: pip3 install flake8, install.packages('lintr')
Plug 'fisadev/vim-isort'             " Python sort imports [dep]: pip3 install isort
Plug 'iamcco/markdown-preview.nvim', { 'do': { -> mkdp#util#install() }, 'for': ['markdown', 'vim-plug']}
Plug 'mhinz/vim-startify'            " A start menu for vim
Plug 'tpope/vim-surround'            " replace surrounding characters
Plug 'filipekiss/ncm2-look.vim'      " ncm2 spelling
Plug 'SirVer/ultisnips'              " hotkeys for chunks of code
Plug 'JuliaEditorSupport/julia-vim'  " julia syntax highlighting
Plug 'tmhedberg/SimpylFold'          " Code folding (zo: open, zc: close)

call plug#end()

" ncm2-loom
let g:ncm2_look_enabled = 0

" turn on spelling and make a spell file
set spelllang=en_us
set spellfile=~/.config/nvim/en.utf-8.add

" startify
let g:startify_lists = [
      \ { 'type': 'sessions',  'header': ['   Sessions']       },
      \ { 'type': 'files',     'header': ['   Recent']            },
      \ { 'type': 'commands',  'header': ['   Commands']       },
      \ ]

" markdown-preview.nvim
let g:mkdp_auto_start = 0
let g:mkdp_auto_close = 1
let g:mkdp_refresh_slow = 0
let g:mkdp_command_for_global = 0
let g:mkdp_open_to_the_world = 0
let g:mkdp_open_ip = ''
let g:mkdp_browser = ''
let g:mkdp_echo_preview_url = 0
let g:mkdp_browserfunc = ''
let g:mkdp_preview_options = {
    \ 'mkit': {},
    \ 'katex': {},
    \ 'uml': {},
    \ 'maid': {},
    \ 'disable_sync_scroll': 0,
    \ 'sync_scroll_type': 'middle',
    \ 'hide_yaml_meta': 1
    \ }
let g:mkdp_markdown_css = ''
let g:mkdp_highlight_css = ''
let g:mkdp_port = ''
let g:mkdp_page_title = '「${name}」'

" vim-isort 
let g:vim_isort_map = '<C-i>'

" Ale Linting
let g:ale_sign_column_always=1
let g:ale_lint_on_enter=1
let g:ale_lint_on_text_changed='always'
let g:ale_echo_msg_error_str='E'
let g:ale_echo_msg_warning_str='W'
let g:ale_echo_msg_format='[%linter%] %s [%severity%]: [%...code...%]'
let g:ale_linters={'python': ['flake8'], 'r': ['lintr']}
let g:ale_fixers={'python': ['black']}

" lightline  -- I am not using lightine actually, but airline ....
" let g:lightline = {
"      \ 'colorscheme': 'wombat',
"      \ 'active': {
"      \   'left': [ [ 'mode', 'paste' ],
"      \             [ 'gitbranch', 'readonly', 'filename', 'modified' ] ]
"      \ },
"      \ 'component_function': {
"      \   'gitbranch': 'FugitiveHead'
"      \ },
"      \ }

" ncm2 
autocmd BufEnter * call ncm2#enable_for_buffer()      " enable ncm2 for all buffers
set completeopt=noinsert,menuone,noselect             " IMPORTANT: :help Ncm2PopupOpen for more information
let g:python3_host_prog='/usr/bin/python3'            " ncm2-jedi


" gitgutter
let g:gitgutter_async=0

" nerdtree settings
map <C-n> :NERDTreeToggle<CR>
let NERDTreeIgnore = ['\.pyc$']  " ignore pyc files

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

" Lua Plugins configuration

lua require('config')

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
" Final theme settings - has to be at the end of config file, dont know why
"
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" colorscheme one                  " use vim-one colorscheme
set background=dark              " [dark or light]

" Tokyonight theme
let g:tokyonight_style = "night"

colorscheme tokyonight

" Colorscheme overrides
"
highlight LineNr ctermfg=99 guifg=#875fff
