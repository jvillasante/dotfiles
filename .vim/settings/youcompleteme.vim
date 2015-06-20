" YouCompleteMe Python
" https://github.com/Valloric/YouCompleteMe/issues/36#issuecomment-40921899
if neobundle#tap('YouCompleteMe')
  function! neobundle#hooks.on_source(bundle)
    let g:ycm_global_ycm_extra_conf = $HOME.'/.ycm_extra_conf.py'
    "nnoremap <silent> <Leader>d :YcmCompleter GoToDefinition<cr>
    nnoremap <silent> <Leader>g :YcmCompleter GoToDefinitionElseDeclaration<cr>
    let g:ycm_goto_buffer_command = 'vertical-split'
    " let g:pymode_rope_goto_definition_bind = '<Leader>g'


    let g:ycm_register_as_syntastic_checker = 1
    let g:ycm_cache_omnifunc = 0
    set completeopt-=preview


    " au FileType python setlocal completeopt-=preview " The reason to deactivate jedi#auto_vim_configuration
    let g:ycm_confirm_extra_conf = 0
    let g:ycm_autoclose_preview_window_after_insertion = 1
    let g:ycm_autoclose_preview_window_after_completion = 1
    let g:ycm_add_preview_to_completeopt = 1
    " let g:ycm_auto_trigger = 0
    let g:ycm_key_invoke_completion = '<C-n>'
    let g:ycm_filetype_blacklist =
          \ get( g:, 'ycm_filetype_blacklist',
          \   get( g:, 'ycm_filetypes_to_completely_ignore', {
          \     'notes' : 1,
          \     'markdown' : 1,
          \     'text' : 1,
          \     'unite' : 1,
          \     'vimfiler' : 1,
          \     'nerdtree' : 1,
          \ } ) )

    " Thanks http://stackoverflow.com/a/22253548
    " make YCM compatible with UltiSnips (using supertab)
    let g:ycm_key_list_select_completion = ['<C-n>', '<Down>']
    let g:ycm_key_list_previous_completion = ['<C-p>', '<Up>']

    " Enable omni completion
    autocmd FileType css,less setlocal omnifunc=csscomplete#CompleteCSS
    autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
    autocmd FileType ejs,jst setlocal omnifunc=htmlcomplete#CompleteTags

    if neobundle#is_installed('marijnh/tern_for_vim')
      autocmd FileType javascript setlocal omnifunc=tern#Complete
    else
      autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
    endif

    autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
    autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags
    autocmd FileType ruby setlocal omnifunc=rubycomplete#Complete
    autocmd FileType java setlocal omnifunc=eclim#java#complete#CodeComplete
    autocmd  FileType  php setlocal omnifunc=phpcomplete_extended#CompletePHP

  endfunction

  call neobundle#untap()
endif
