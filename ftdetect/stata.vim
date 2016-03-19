
" Stata do and ado files
au BufNewFile,BufRead *.do     set filetype=stata
au BufNewFile,BufRead *.ado    set filetype=stata
" *.doi is listed because some people use the extension to indicate
" files run from within other files (e.g. with the include command).
au BufNewFile,BufRead *.doi    set filetype=stata

" Stata help files
au BufNewFile,BufRead *.sthlp  set filetype=smcl
au BufNewFile,BufRead *.ihlp   set filetype=smcl
au BufNewFile,BufRead *.smcl   set filetype=smcl

" Stata dictionary files
au BufNewFile,BufRead *.dct   set filetype=statadct
