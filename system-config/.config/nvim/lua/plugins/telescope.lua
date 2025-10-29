return {
  {
    'nvim-telescope/telescope.nvim',
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
      {
        "<Leader>fd",
        function()
          require("telescope.builtin").diagnostics { severity_limit = vim.diagnostic.severity.WARN }
        end,
      },
      { "<Leader>fk", "<Cmd>Telescope find_files cwd=~/mediary-system<CR>" },
      { "<Leader>fc", "<Cmd>Telescope find_files cwd=~/.config/nvim follow=true<CR>" },
      {
        "<Leader>fw",
        function()
          require("telescope.builtin").find_files { cwd = require("config.util").get_buf_cwd(), follow = true }
        end,
      },
      { "<Leader>fh", "<Cmd>Telescope oldfiles<CR>" },
      { "<Leader>fb", "<Cmd>Telescope buffers<CR>" },
      { "<Leader>fm", "<Cmd>Telescope man_pages<CR>" },
      { "<Leader>fl", "<Cmd>Telescope current_buffer_fuzzy_find<CR>" },
      { "<Leader>fT", "<Cmd>Telescope treesitter<CR>" },
      -- Extensions
      { "<Leader>fu", "<Cmd>Telescope undo<CR>" },
    },
  },
}
