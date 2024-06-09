" Mappings and functions are allowed to clobber the z mark or register.

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Plugins
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

call plug#begin('~/.config/nvim/plugged')

Plug 'cdchawthorne/nvim-tbufferline'
Plug 'ciaranm/inkpot'

Plug 'kylechui/nvim-surround'
" lua require("nvim-surround").setup()

call plug#end()

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Options
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

source ~/.config/nvim/lua/init.lua

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Autocommands
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

augroup restore_cursor_pos
  autocmd!

  autocmd BufReadPost *
      \ if line("'\"") > 1 && line("'\"") <= line("$")
      \|  execute "normal! g`\""
      \|endif
augroup END
