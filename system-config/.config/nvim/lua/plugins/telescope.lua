local util = require('config.util')

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
      { "<Leader>fk", "<Cmd>Telescope find_files cwd=~/work/mediary-system<CR>" },
      { "<Leader>fn", "<Cmd>Telescope find_files cwd=~/.config/nvim follow=true<CR>" },
      {
        "<Leader>fo",
        function()
          require("telescope.builtin").find_files { cwd = util.get_buf_cwd(), follow = true }
        end,
      },
      { "<Leader>fg", "<Cmd>Telescope live_grep cwd=~/mediary-system additional_args=-i<CR>" },
      { "<Leader>fh", "<Cmd>Telescope oldfiles<CR>" },
      { "<Leader>fm", "<Cmd>Telescope man_pages<CR>" },
      { "<Leader>fl", "<Cmd>Telescope current_buffer_fuzzy_find<CR>" },
      { "<Leader>fr", "<Cmd>Telescope resume<CR>" },
      { "<Leader>f;", "<Cmd>Telescope command_history<CR>" },
      { "<Leader>f/", "<Cmd>Telescope search_history<CR>" },
      -- buffers
      { "<Leader>kl", "<Cmd>Telescope buffers show_all_buffers=false ignore_current_buffer=true sort_mru=true cwd=/home/cdchawthorne/mediary-system<CR>" },
      -- git
      {
        "<Leader>gs",
        function()
          require("telescope.builtin").git_stash { cwd = util.get_buf_cwd() }
        end,
      },
      { "<Leader>gh", "<Cmd>Telescope git_bcommits use_file_path=true<CR>" },
      { "<Leader>gh", "<Cmd>Telescope git_bcommits_range use_file_path=true<CR>", mode = 'v' },
      -- Extensions
      { "<Leader>fu", "<Cmd>Telescope undo<CR>" },
    },
  },
}
