--------------------------------------------------
--
-- Theming
--
------------------------------------------------

-- tokyonight theme settings
vim.g.tokyonight_colors = { comment = "#00af5f"}

---------------------------------------------------
--
-- General highlighting
--
---------------------------------------------------

--
-- tree-sitter setup
--

-- Specific settings for norg parser

-- parser_configs.norg = {
--     install_info = {
--         url = "https://github.com/nvim-neorg/tree-sitter-norg",
--         files = { "src/parser.c", "src/scanner.cc" },
--         branch = "main"
--     },
-- }

-- parser_configs.norg_meta = {
--     install_info = {
--         url = "https://github.com/nvim-neorg/tree-sitter-norg-meta",
--         files = { "src/parser.c" },
--         branch = "main"
--     },
-- }

-- parser_configs.norg_table = {
--     install_info = {
--         url = "https://github.com/nvim-neorg/tree-sitter-norg-table",
--         files = { "src/parser.c" },
--         branch = "main"
--     },
-- }

-- 21. 11. 2022: MacOS M1 troubleshoting:
--
-- neovim is best installed with the x86_64 architecture, so with `brew` install with:
--
-- ```
-- arch -x86_64 /usr/local/bin/brew install package
-- ```

-- install tree-sitter parsers, "maintained" + norg specifically

require'nvim-treesitter.configs'.setup {
 ensure_installed = { "bash", "bibtex", "c", "clojure", "cmake", "commonlisp", "cpp", "css", "haskell", "html", "json", "julia", "latex", "lua", "nix", "python", "regex", "rust", "toml", "yaml", "markdown" },  -- one of "all", (parsers with maintainers), or a list of languages
  -- 21. 11. 2022: Removed: "norg", "norg_meta", "norg_table"
  -- "norg" is not among "maintained" languages, so list of languages is needed
  ignore_install = { "c_sharp", "cuda" }, -- List of parsers to ignore installing
  highlight = {
    enable = true,              -- false will disable the whole extension
    disable = { "markdown" },  -- list of language that will be disabled
  },
  incremental_selection = {
    enable = true,
    keymaps = {
      init_selection = "gnn",
      node_incremental = "grn",
      scope_incremental = "grc",
      node_decremental = "grm",
    },
  },
}

-- vim-colorizer setup

-- Attaches to every FileType mode
require 'colorizer'.setup()

---------------------------------------------------------
--
-- Keybindings
--
--------------------------------------------------------

-- which-key setup

-- require("which-key").setup {
  -- your configuration comes here
  -- or leave it empty to use the default settings
  -- refer to the configuration section
-- }

--------------------------------------------------------
--
-- Completion
--
--------------------------------------------------------

-- Completion settings
require'compe'.setup {
  enabled = true;
  autocomplete = true;
  source = {
    path = true;
    buffer = true;
    calc = true;
    nvim_lsp = true;
    neorg = true;
  };
}

--------------------------------------------------------
--
-- Code writing
--
--------------------------------------------------------

--
-- Git setup
--

-- neogit setup -- disabled for now, the classic fugitive seems to be superior

--local neogit = require('neogit')

--neogit.setup {}

--------------------------------------------------------
--
-- Text linting
--
-------------------------------------------------------

local null_ls = require("null-ls")

null_ls.setup({
    sources = {
        null_ls.builtins.diagnostics.alex.with({
             filetypes = {"html", "yaml"}
         }),
        null_ls.builtins.diagnostics.chktex,
        null_ls.builtins.diagnostics.markdownlint,
        null_ls.builtins.diagnostics.markdownlint_cli2,
        null_ls.builtins.diagnostics.shellcheck,
        -- null_ls.builtins.diagnostics.spectral, 21. 11. 2022: Seems to be bugged
        null_ls.builtins.diagnostics.vale,
        null_ls.builtins.diagnostics.write_good
    },
})

require("trouble").setup {
  -- your configuration comes here
  -- or leave it empty to use the default settings
  -- refer to the configuration section below
}
