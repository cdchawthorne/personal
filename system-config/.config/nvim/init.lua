local opt = vim.opt
local cmd = vim.cmd
local fn = vim.fn
local api = vim.api
local g = vim.g
local b = vim.b
local map = vim.keymap.set

--------------
-- Settings --
--------------

opt.backup = true
opt.backupdir = fn.stdpath("data") .. "/backup"
opt.breakat = " "
opt.colorcolumn = "+1"
-- opt.completeopt:append { "longest" }
opt.diffopt:append { "iwhite", "foldcolumn:0", "context:1000000", "vertical" }
opt.expandtab = true
opt.foldenable = false
opt.guicursor = { "n-v-c-sm:block", "i-ci-ve-r-cr-o:hor20", "a:blinkon500-blinkoff500" }
opt.hidden = true
opt.ignorecase = true
opt.inccommand = "split"
opt.laststatus = 3
opt.matchtime = 1
opt.mouse = ""
opt.modeline = false
opt.number = true
opt.relativenumber = true
opt.scrollback = 100000
opt.shiftwidth = 2
opt.showcmd = true
opt.showmatch = true
opt.smartcase = true
opt.softtabstop = 2
opt.spelllang = "en_ca"
opt.splitbelow = true
opt.splitright = true
opt.tabstop = 2
opt.tags = fn.stdpath("data") .. "/tags"
opt.tildeop = true
opt.timeoutlen = 1000
opt.undofile = true
opt.undodir = fn.stdpath("data") .. "/undo"
opt.wildmode = { "longest", "list", "full" }

cmd.language("en_US.utf8")

vim.diagnostic.config {
  severity_sort = true,
  virtual_text = { severity = { min = vim.diagnostic.severity.INFO }},
  signs = { severity = { min = vim.diagnostic.severity.INFO }},
}

--------------
-- autocmds --
--------------

local skeleton_au_group = api.nvim_create_augroup("skeleton_files", {})
api.nvim_create_autocmd("BufNewFile", {
  pattern = "*.tex",
  group = skeleton_au_group,
  command = "0read " .. fn.stdpath("config") .. "/skeleton.tex | normal! Gdd6k$",
})
api.nvim_create_autocmd("BufNewFile", {
  pattern = "*.js",
  group = skeleton_au_group,
  command = "0read " .. fn.stdpath("config") .. "/skeleton.js | normal! G",
})

local terminal_au_group = api.nvim_create_augroup("terminal", {})
api.nvim_create_autocmd("TermOpen", {
  group = terminal_au_group,
  callback = function()
    vim.opt_local.number = false
    vim.opt_local.relativenumber = false
  end,
})
-- Complement the builtin automcd in $VIMRUNTIME/lua/vim/_defaults.lua
api.nvim_create_autocmd({ 'TermClose' }, {
  group = nvim_terminal_augroup,
  nested = true,
  desc = 'Automatically close terminal buffers when started with no arguments and exiting with an error',
  callback = function(args)
    if vim.v.event.status == 0 then
      return
    end
    local info = api.nvim_get_chan_info(vim.bo[args.buf].channel)
    local argv = info.argv or {}
    if table.concat(argv, ' ') == vim.o.shell then
      api.nvim_buf_delete(args.buf, { force = true })
    end
  end,
})

api.nvim_exec2([[
  augroup restore_cursor_pos
    autocmd!

    autocmd BufReadPost *
        \ if line("'\"") > 1 && line("'\"") <= line("$")
        \|  execute "normal! g`\""
        \|endif
  augroup END
]], {})

-----------------------
-- Utility functions --
-----------------------

local function get_buf_cwd()
  if b.terminal_job_pid == nil then
    return fn.expand("%:p:h")
  else
    -- Doing this using nvim's system() since apparently lua only lets you execute
    -- external processes as strings and I really don't want to worry about escaping.
    local dir = fn.system({ "realpath", "/proc/" .. b.terminal_job_pid .. "/cwd" })
    return dir:sub(1, -2)
  end
end

local function spawn_shell(layout_callback)
  local cwd = get_buf_cwd()
  layout_callback()
  fn.termopen({ os.getenv("SHELL") }, { cwd = cwd })
  cmd.startinsert()
end

-- Mappings
local basic_modes = {'n', 'v', 's', 'o'}
local silent = { silent = true }

map(basic_modes, '/', [[/\v]])
map(basic_modes, '?', [[?\v]])
map(basic_modes, '<Tab>', '%')
map(basic_modes, 'v', '<C-b>')
map(basic_modes, 'm', '<C-f>')

map('n', 'S', 'm')
map('n', 'x', ':write<CR>', silent)
map('n', '<Up>', '<Cmd>.m-2<CR>', silent)
map('n', '<Down>', '<Cmd>.m+1<CR>', silent)

map('n', '}f', '<Cmd>cn<CR>', silent)
map('n', '{f', '<Cmd>cp<CR>', silent)

map({'v', 's'}, '<', '<gv')
map({'v', 's'}, '>', '>gv')

local swaps = {
  ['0'] = ')',
  ['9'] = '(',
  ['8'] = '*',
  ['7'] = '&',
  ['6'] = '^',
  ['['] = '{',
  [']'] = '}',
}
for i, v in pairs(swaps) do
  map(basic_modes, i, v)
  map(basic_modes, v, i)
end

map('t', ',.', [[<C-\><C-n>]])

for _, fst in ipairs({ 'f', 'F' }) do
  for _, snd in ipairs({ 'j', 'J' }) do
    map('i', fst .. snd, '<C-]><Esc>')
  end
end

map('i', '<C-u>', '<C-g>u<C-u>')
map('i', 'fdn', '<C-n>')
map('i', 'fdp', '<C-p>')
map('i', 'fdh', '<C-]><Cmd>nohlsearch<CR>')
map('i', 'fdd', '<C-g>u<C-R>=strftime("%Y-%m-%d")<CR>')
map('i', 'j', function() return fn.pumvisible() == 1 and "<C-n>" or "j" end, { expr = true })
map('i', 'k', function() return fn.pumvisible() == 1 and "<C-p>" or "k" end, { expr = true })
map('i', '<Tab>', function() return fn.pumvisible() == 1 and "<C-x><C-o>" or "<Tab>" end, { expr = true })

-- Filetype-specific mappings
local mappings_quickfix_au_group = api.nvim_create_augroup("mappings_quickfix", {})
api.nvim_create_autocmd({ 'FileType' }, {
  pattern = "qf",
  group = mappings_quickfix_au_group,
  callback = function()
    map('n', 'o', '<CR><Cmd>copen<CR>', { silent = true, buffer = true })
    map('n', '<CR>', '<CR><Cmd>cclose<CR>', { silent = true, buffer = true })
  end,
})

-- Leader mappings
g.mapleader = 's'
g.maplocalleader = g.mapleader .. 'l'

map(basic_modes, '<Leader>', '<NOP>')
map(basic_modes, '<LocalLeader>', '<NOP>')

-- Config files
map(basic_modes, '<Leader>r', '<NOP>')
map('n', '<Leader>re', '<Cmd>edit $MYVIMRC<CR>', silent)
map('n', '<Leader>rk', '<Cmd>edit stdpath("config") . "/skeleton.%:e"<CR>', silent)
function edit_ftplugin()
  cmd.edit(stdpath("config") .. "/after/ftplugin/" .. opt.filetype:get() .. ".vim")
end
map('n', '<Leader>ra', edit_ftplugin, silent)

-- Settings
map(basic_modes, '<Leader>s', '<NOP>')
map('n', '<Leader>sh', '<Cmd>nohlsearch<CR>', silent)
map('n', '<Leader>sd', '<Cmd>diffoff!<CR>:bdelete<CR>:bdelete<CR>', silent)
map('n', '<Leader>so', '<Cmd>windo diffthis<CR>', silent)
map('n', '<Leader>sl', '<Cmd>diffoff!<CR><C-w>h:bdelete<CR>', silent)
map('n', '<Leader>sn', [[<Cmd>set number! | set relativenumber!<CR>]], silent)
map('n', '<Leader>ss', '<Cmd>syntax sync fromstart<CR>', silent)

-- Buffers
map(basic_modes, '<Leader>k', '<NOP>')
map(basic_modes, '<Leader>ks', '<NOP>')
map(basic_modes, '<Leader>kv', '<NOP>')

map('n', '<Leader>kf', '<C-^>')
map('n', '<Leader>kc', function() spawn_shell(cmd.enew) end, { silent = true })
map('n', '<Leader>ksc', function() spawn_shell(cmd.new) end, silent)
map('n', '<Leader>ksf', '<Cmd>sbuffer #<CR>', silent)
map('n', '<Leader>kvc', function() spawn_shell(cmd.vnew) end, silent)
map('n', '<Leader>kvf', '<Cmd>vertical sbuffer #<CR>', silent)
map('n', '<Leader>kd', '<Cmd>bdelete<CR>', silent)
map('n', '<Leader>ko', function() return 'q:aedit ' .. get_buf_cwd() .. "/" end, { expr = true })
map('n', '<Leader>kl', [[i<C-l><C-\><C-n><Cmd>set scrollback=1 | set scrollback=100000<CR>]], silent)

-- Windows
map(basic_modes, '<Leader>d', '<C-w>')
map(basic_modes, '<Leader>dd', '<C-w><C-w>')

-- Leader miscellany
map({ 'n', 'v', 's' }, '<Leader>;', 'q:i')
map({ 'n', 'v', 's' }, '<Leader>/', ':tag ')
map({ 'n', 'v' }, '<Leader>.', 'G')
map({ 'n', 'v' }, '<Leader>,', 'gg')
map(basic_modes, '<Leader>v', 'v')
map('n', '<Leader>c', '0D')
map('n', '<Leader>a', '<Cmd>qa<CR>')
map('n', '<Leader>o', 'o<C-u>')
map('n', '<Leader>O', 'O<C-u>')

--------------
-- Commands --
--------------

api.nvim_create_user_command(
  'DiffOrig',
  'vertical new | set buftype=nofile | read # | 0delete_ | diffthis | wincmd p | diffthis',
  {}
)

-------------
-- Plugins --
-------------

local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

require("lazy").setup({
  {
    "cdchawthorne/inkpot",
    init = function()
      g.inkpot_black_background = true
    end,
    config = function()
      opt.termguicolors = false
      cmd.colorscheme("inkpot")
    end,
  },
  {
    "cdchawthorne/nvim-tbufferline",
    lazy = false,
    -- dev = true,
    init = function()
      g.tbufferline_enable_on_startup = 1
    end,
    keys = {
      { '<Space>', '<Plug>tbufferline#Buffer' },
      { '<Leader>ksl', '<Plug>tbufferline#SplitBuffer', silent = true },
      { '<Leader>kvl', '<Plug>tbufferline#VSplitBuffer', silent = true },
      { '<Leader>kj', '<Plug>tbufferline#StepForward', silent = true },
      { '<Leader>kk', '<Plug>tbufferline#StepBack', silent = true },
    },
  },
  {
    "kylechui/nvim-surround",
    config = true,
  },
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
  {
    'mbbill/undotree',
    init = function()
      g.undotree_WindowLayout = 3
      g.undotree_SetFocusWhenToggle = 1
    end,
    keys = {
      { "<Leader>su", "<Cmd>UndotreeToggle<CR>" },
    },
  },
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
  -- WARNING: see https://github.com/nvim-treesitter/nvim-treesitter/issues/3092
  -- Basically if it chokes on vimdocs run :TSInstall! vimdoc
  -- {
  --   "nvim-treesitter/nvim-treesitter",
  --   build = ":TSUpdate",
  -- },
  {
    'neovim/nvim-lspconfig',
    lazy = false,
    config = function()
      require('lspconfig').rust_analyzer.setup {
        settings = {
          ['rust-analyzer'] = {
            -- cargo = { features = { 'veea' }, },
            check = {
              -- features = { 'veea' },
              command = 'clippy',
              ignore = {
                'clippy::too_many_arguments',
                'clippy::large_enum_variant',
                'clippy::new_without_default',
                'clippy::len_without_is_empty',
                'clippy::needless_range_loop',
              },
            },
            diagnostics = { disabled = { 'inactive-code' } },
          },
        }
      }
      -- disable lsp-based syntax highlighting; see
      -- https://www.reddit.com/r/neovim/comments/109vgtl/comment/j40jzjd/ and
      -- https://github.com/simrat39/rust-tools.nvim/issues/365#issuecomment-1506286437
      -- (and :h lspconfig-setup and :h LspAttach)
      api.nvim_create_autocmd("LspAttach", {
        callback = function(args)
          -- local bufnr = args.buf
          local client = vim.lsp.get_client_by_id(args.data.client_id)
          client.server_capabilities.semanticTokensProvider = nil
        end,
      })
      api.nvim_create_autocmd("CompleteDone", { command = "pclose" })
    end,
    keys = {
      { '<Leader>jr', function() vim.lsp.buf.rename() end, desc = "rename" },
      -- { '<Leader>jc', function() vim.lsp.buf.incoming_calls() end },
      { '<Leader>jc', function() vim.lsp.buf.references() end, desc = "list references" },
      { '<Leader>jf', function() vim.lsp.buf.code_action() end, desc = "fix code" },
      { '<Leader>jj', function() vim.lsp.buf.format() end, desc = "format" },
      {
        '<Leader>jt',
        function() vim.diagnostic.enable(not vim.diagnostic.is_enabled()) end,
        desc = "toggle diagnostics",
      },
      { '<Leader>jo', function() vim.diagnostic.open_float() end, desc = "show diagnostic" },
      {
        '<Leader>jR',
        function()
          vim.lsp.stop_client(vim.lsp.get_clients())
          vim.cmd.edit()
        end,
        desc = "restart LSP",
      },
      { '<Leader>jl', '<Cmd>cclose<CR>', desc = "exit quickfix window" },
      { 
        '<Leader>jh',
        function() vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled()) end,
        desc = "toggle inlay hints",
      },
      {
        '{h',
        function()
          vim.diagnostic.goto_prev { severity = { max = vim.diagnostic.severity.INFO } }
        end,
        desc = "prev diagnostic hint",
      },
      {
        '}h',
        function()
          vim.diagnostic.goto_next { severity = { max = vim.diagnostic.severity.INFO } }
        end,
        desc = "next diagnostic hint",
      },
      {
        '{d',
        function()
          vim.diagnostic.goto_prev { severity = { min = vim.diagnostic.severity.WARN } }
        end,
        desc = "prev diagnostic",
      },
      {
        '}d',
        function()
          vim.diagnostic.goto_next { severity = { min = vim.diagnostic.severity.WARN } }
        end,
        desc = "next diagnostic",
      },
      { 'fdk', '<C-x><C-o>', mode = 'i', desc = "autocompletion" },
      { '<C-k>', function() vim.lsp.buf.type_definition() end, desc = "jump to type definition" },
    },
  }
}, {
  dev = { path = "~/repos" },
})
