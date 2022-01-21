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

local parser_configs = require('nvim-treesitter.parsers').get_parser_configs()

-- Specific settings for norg parser

parser_configs.norg = {
    install_info = {
        url = "https://github.com/nvim-neorg/tree-sitter-norg",
        files = { "src/parser.c", "src/scanner.cc" },
        branch = "main"
    },
}

parser_configs.norg_meta = {
    install_info = {
        url = "https://github.com/nvim-neorg/tree-sitter-norg-meta",
        files = { "src/parser.c" },
        branch = "main"
    },
}

parser_configs.norg_table = {
    install_info = {
        url = "https://github.com/nvim-neorg/tree-sitter-norg-table",
        files = { "src/parser.c" },
        branch = "main"
    },
}

-- install tree-sitter parsers, "maintained" + norg specifically

require'nvim-treesitter.configs'.setup {
  ensure_installed = { "bash", "bibtex", "c", "clojure", "cmake", "commonlisp", "cpp", "css", "fish", "haskell", "html", "java", "json", "julia", "kotlin", "latex", "lua", "nix", "python", "r", "regex", "rust", "scala", "toml", "yaml", "norg", "norg_meta", "norg_table" },  -- one of "all", "maintained" (parsers with maintainers), or a list of languages
  -- "norg" is not among "maintained" languages, so list of languages is needed
  ignore_install = { "c_sharp", "cuda" }, -- List of parsers to ignore installing
  highlight = {
    enable = true,              -- false will disable the whole extension
    disable = { },  -- list of language that will be disabled
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

require("which-key").setup {
  -- your configuration comes here
  -- or leave it empty to use the default settings
  -- refer to the configuration section
}

---------------------------------------------------------
--
-- General Writing
--
--------------------------------------------------------

-- Neorg

-- Basic neorg settings per https://github.com/nvim-neorg/neorg

require('neorg').setup {
  -- Tell Neorg what modules to load
  load = {
      ["core.defaults"] = {}, -- Load all the default modules
      ["core.norg.concealer"] = {}, -- Allows for use of icons
      ["core.norg.dirman"] = { -- Manage your directories with Neorg
          config = {
              workspaces = {
                  my_workspace = "~/neorg"
              },
              engine = "nvim-compe",
          }
      }
  },
}

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

--
-- LSP settings
--

-- LSP Installer settings
local function setup_servers()
  require'lspinstall'.setup()
  local servers = require'lspinstall'.installed_servers()
  for _, server in pairs(servers) do
    require'lspconfig'[server].setup{}
  end
end

setup_servers()

-- Automatically reload after `:LspInstall <server>` so we don't have to restart neovim
require'lspinstall'.post_install_hook = function ()
  setup_servers() -- reload installed servers
  vim.cmd("bufdo e") -- this triggers the FileType autocmd that starts the server
end

-- configuration of comments input

require('kommentary.config').configure_language("rust", {
    single_line_comment_string = "//",
    multi_line_comment_strings = {"/*", "*/"},
})
