return {
  {
    'lewis6991/gitsigns.nvim',
    lazy = false,
    -- opts = { base = "HEAD" },
    config = true,
    keys = {
      { '<Leader>gt', '<Cmd>Gitsigns toggle_signs<CR>', silent = true },
      { '<Leader>gd', '<Cmd>Gitsigns diffthis<CR>', silent = true },
      { '<Leader>gb', '<Cmd>Gitsigns blame_line<CR>', silent = true },
      { '<Leader>gB', '<Cmd>Gitsigns blame<CR>', silent = true },
      { '<Leader>gk', '<Cmd>Gitsigns stage_hunk<CR>', silent = true },
      { '<Leader>gl', '<Cmd>Gitsigns preview_hunk_inline<CR>', silent = true },
      {
        'do',
        function()
          if vim.wo.diff then
            vim.cmd.normal({'do', bang = true})
          else
            require("gitsigns").reset_hunk()
          end
        end,
      },
      {
        '}c',
        function()
          if vim.wo.diff then
            vim.cmd.normal({']c', bang = true})
          else
            require("gitsigns").nav_hunk('next')
          end
        end,
      },
      {
        '{c',
        function()
          if vim.wo.diff then
            vim.cmd.normal({'[c', bang = true})
          else
            require("gitsigns").nav_hunk('prev')
          end
        end,
      },
    }
  },
}
