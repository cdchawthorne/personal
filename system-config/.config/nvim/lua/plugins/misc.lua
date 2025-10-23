return {
  {
    "cdchawthorne/inkpot",
    init = function()
      vim.g.inkpot_black_background = true
    end,
    config = function()
      vim.opt.termguicolors = false
      vim.cmd.colorscheme("inkpot")
    end,
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
    "AndrewRadev/sideways.vim",
    keys = {
      { "<Left>", "<Cmd>SidewaysLeft<CR>", silent = true },
      { "<Right>", "<Cmd>SidewaysRight<CR>", silent = true },
      { "aa", "<Plug>SidewaysArgumentTextobjA", mode = { 'o', 'x' }, silent = true },
      { "ia", "<Plug>SidewaysArgumentTextobjI", mode = { 'o', 'x' }, silent = true },
    },
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
