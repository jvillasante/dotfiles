if neobundle#tap('vim-airline')
  function! neobundle#hooks.on_source(bundle)
    set laststatus=2
    let g:airline_powerline_fonts=1
    if !exists('g:airline_symbols')
      let g:airline_symbols = {}
    endif
    let g:airline_symbols.space = "\ua0"
    let g:airline#extensions#tabline#enabled = 1
    let g:airline#extensions#tabline#show_buffers = 0
    let g:airline#extensions#tabline#fnamemod=':t'
    let g:airline#extensions#tabline#tab_nr_type = 1 " tab number
    let g:airline_theme='solarized'
    " let g:airline_detect_iminsert = 1
  endfunction

  call neobundle#untap()
endif
