function! <SID>SynStack()
  if !exists("*synstack")
    return
  endif
  " echo map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")')
  echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<'
  \ . synIDattr(synID(line("."),col("."),0),"name") . "> lo<"
  \ . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"
endfunc

nmap <leader>I :call <SID>SynStack()<CR>
