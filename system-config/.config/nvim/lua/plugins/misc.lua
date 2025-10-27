return {
  {
    "folke/tokyonight.nvim",
    lazy = false,
    priority = 1000,
    opts = {
      style = "night",
      terminal_colors = false,
      on_colors = function(colors)
        colors.fg = "#ffffff"
        colors.bg = "#000000"
      end,
      on_highlights = function(highlights, colors)
        highlights.Tbufferline = { bold = true, fg = "#9e9e9e", bg = "#262626" }
        highlights.TbufferlineNC = { fg = "#9e9e9e", bg = "#262626" }
      end
    },
    config = function(_, opts)
      require("tokyonight").setup(opts)
      vim.cmd.colorscheme("tokyonight")
    end
  },
  {
    "cdchawthorne/nvim-tbufferline",
    lazy = false,
    init = function()
      vim.g.tbufferline_enable_on_startup = 1
    end,
    keys = {
      { '<Space>', '<Plug>tbufferline#Buffer' },
      { '<Leader>ksl', '<Plug>tbufferline#SplitBuffer', silent = true },
      { '<Leader>kvl', '<Plug>tbufferline#VSplitBuffer', silent = true },
      { '<Leader>kj', '<Plug>tbufferline#StepForward', silent = true },
      { '<Leader>kk', '<Plug>tbufferline#StepBack', silent = true },
    },
  },
  { "kylechui/nvim-surround", opts = {} },
  {
    "ggandor/leap.nvim",
    keys = {
      { 'f', '<Plug>(leap-forward)', mode = { 'n', 'x' }, },
      { 'F', '<Plug>(leap-backward)', mode = { 'n', 'x' }, },
      { 'gf', '<Plug>(leap-from-window)', mode = { 'n', 'x' }, },
    },
    config = function()
      require('leap.user').set_repeat_keys('<enter>', '<backspace>')
    end,
  },
  {
    'mbbill/undotree',
    init = function()
      vim.g.undotree_WindowLayout = 3
      vim.g.undotree_SetFocusWhenToggle = 1
    end,
    keys = {
      { "<Leader>su", "<Cmd>UndotreeToggle<CR>" },
    },
  },
  'neovim/nvim-lspconfig',
}
