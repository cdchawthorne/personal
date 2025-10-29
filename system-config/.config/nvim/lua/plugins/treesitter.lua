local auto_installed = { 'c', 'vim' }
local ensure_installed = { 'rust' }

local nvim_treesitter = {
  "nvim-treesitter/nvim-treesitter",
  lazy = false,
  branch = "main",
  build = ':TSUpdate',
  config = function()
    local nvim_treesitter = require('nvim-treesitter')
    local installed = nvim_treesitter.get_installed()
    local to_install = vim.tbl_filter(
      function(lang) return not vim.list_contains(installed, lang) end,
      ensure_installed
    )
    if #to_install > 0 then
      nvim_treesitter.install(to_install, { summary = true })
    end
  end
}

local nvim_treesitter_textobjects = {
  "nvim-treesitter/nvim-treesitter-textobjects",
  lazy = false,
  branch = "main",
  dependencies = { "nvim-treesitter/nvim-treesitter" },
  opts = {},
  keys = {
    { 'ia', function() require('nvim-treesitter-textobjects.select').select_textobject('@parameter.inner') end, mode = { 'x', 'o' } },
    { 'aa', function() require('nvim-treesitter-textobjects.select').select_textobject('@parameter.outer') end, mode = { 'x', 'o' } },
    { '<Left>', function() require('nvim-treesitter-textobjects.swap').swap_previous('@parameter.inner') end },
    { '<Right>', function() require('nvim-treesitter-textobjects.swap').swap_next('@parameter.inner') end },
  }
}

local nvim_treesitter_context = {
  "nvim-treesitter/nvim-treesitter-context",
  opts = { max_lines = 3 },
  keys = {
    { "{t", function() require("treesitter-context").go_to_context(vim.v.count1) end, silent = true },
  },
}

vim.api.nvim_create_autocmd('FileType', {
  group = vim.api.nvim_create_augroup('treesitter', {}),
  callback = function(ev)
    if vim.list_contains(ensure_installed, ev.match) or vim.list_contains(auto_installed, ev.match) then
      vim.treesitter.start()
      vim.bo.indentexpr = "v:lua.require'nvim-treesitter'.indentexpr()"
    end
  end,
})

return { nvim_treesitter, nvim_treesitter_textobjects, nvim_treesitter_context }
