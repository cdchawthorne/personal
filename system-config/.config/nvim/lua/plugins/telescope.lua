return {
  {
    'nvim-telescope/telescope.nvim',
    branch = '0.1.x',
    dependencies = {
      'nvim-lua/plenary.nvim',
      'debugloop/telescope-undo.nvim',
    },
    opts = {
      defaults = {
        layout_config = {
          preview_cutoff = 0,
          width = 0.95,
          height = 0.95,
        },
      },
      extensions = {
        undo = {
          side_by_side = true,
          layout_strategy = 'horizontal',
          layout_config = {
            preview_width = { padding = 15 },
            -- width = { padding = 0 },
            -- height = { padding = 0 },
          },
          entry_format = "#$ID, $TIME",
        },
      },
    },
    config = function(_, opts)
      require("telescope").setup(opts)
      require("telescope").load_extension("undo")
    end,
    keys = {
      { "<Leader>fa", "<Cmd>Telescope find_files<CR>" },
      {
        "<Leader>fd",
        function()
          require("telescope.builtin").diagnostics { severity_limit = vim.diagnostic.severity.WARN }
        end,
      },
      { "<Leader>fk", "<Cmd>Telescope find_files cwd=~/mediary-system<CR>" },
      { "<Leader>fh", "<Cmd>Telescope oldfiles<CR>" },
      { "<Leader>fb", "<Cmd>Telescope buffers<CR>" },
      { "<Leader>fm", "<Cmd>Telescope man_pages<CR>" },
      {
        "<Leader>ft",
        function() 
          require('telescope.builtin').tags({ ctags_file = opt.tags:get()[1] })
        end,
      },
      { "<Leader>fl", "<Cmd>Telescope current_buffer_fuzzy_find<CR>" },
      { "<Leader>fT", "<Cmd>Telescope treesitter<CR>" },
      -- Extensions
      { "<Leader>fu", "<Cmd>Telescope undo<CR>" },
    },
  },
}
