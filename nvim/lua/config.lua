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

-- tree-sitter setup

require'nvim-treesitter.configs'.setup {
  ensure_installed = "maintained", -- one of "all", "maintained" (parsers with maintainers), or a list of languages
  ignore_install = { "c_sharp", "cuda" }, -- List of parsers to ignore installing
  highlight = {
    enable = true,              -- false will disable the whole extension
    disable = { },  -- list of language that will be disabled
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
