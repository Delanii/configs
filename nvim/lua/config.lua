-- vim-colorizer setup

-- Attaches to every FileType mode
require 'colorizer'.setup()

-- neogit setup

local neogit = require('neogit')

neogit.setup {}

-- tree-sitter setup
require'nvim-treesitter.configs'.setup {
  ensure_installed = "maintained", -- one of "all", "maintained" (parsers with maintainers), or a list of languages
  ignore_install = { "c_sharp", "cuda" }, -- List of parsers to ignore installing
  highlight = {
    enable = true,              -- false will disable the whole extension
    disable = { },  -- list of language that will be disabled
  },
}
