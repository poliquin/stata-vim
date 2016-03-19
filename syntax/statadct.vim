" Vim syntax file for Stata dictionary files
" Language: Stata and/or Mata
" Maintainer: Chris Poliquin <chrispoliquin@gmail.com>
" Last Change: 2015-09-07
" Version: 0.1.0

" Log:
" 2015-09-07    Created file


"if version < 600
"    syntax clear
"elseif exists("b:current_syntax")
"    finish
"endif

syntax case match


" Notes
syn keyword stataTodo TODO FIXME XXX contained


" Comments
syn region stataStarComment  start=/\s*\*/ end=/$/   contains=stataComment,stataTodo oneline
syn region stataSlashComment start="\s//"  end=/$/   contains=stataComment,stataTodo oneline
syn region stataSlashComment start="^//"   end=/$/   contains=stataComment,stataTodo oneline
" multi-line
syn region stataComment      start="/\*"   end="\*/" contains=stataComment,stataTodo


" Variable formats
" numbers
syn match stataFormat /%\(\|\d\+\|\d\+\.\d\+\)[efg]/
" strings
syn match stataFormat /%\(\|\d\+\)[sS]/


" Commands
syn keyword stataCommand clear
syn keyword stataCommand dictionary
syn keyword stataCommand infile
syn keyword stataCommand infix
syn keyword stataCommand using


" Variable Types
syn keyword stataType byte
syn keyword stataType int
syn keyword stataType long
syn keyword stataType float
syn keyword stataType double
syn match   stataType /strL\?[0-9]*/


" Literals
syn match  stataQuote   /"/
syn region stataString  start=/"/  end=/"/  oneline
syn region stataEString matchgroup=Nothing start=/`"/ end=/"'/ oneline contains=stataQuote,stataEString,stataString


" Functions
syn match  stataInt /\d\+/ contained
syn region stataFunc matchgroup=Function start=/\<_lrecl(/ end=/)/ contains=stataInt
syn region stataFunc matchgroup=Function start=/\<_firstlineoffile(/ end=/)/ contains=stataInt
syn region stataFunc matchgroup=Function start=/\<_first(/ end=/)/ contains=stataInt
syn region stataFunc matchgroup=Function start=/\<_lines(/ end=/)/ contains=stataInt
syn region stataFunc matchgroup=Function start=/\<_line(/ end=/)/ contains=stataInt
syn region stataFunc matchgroup=Function start=/\<_newline(/ end=/)/ contains=stataInt
syn region stataFunc matchgroup=Function start=/\<_column(/ end=/)/ contains=stataInt
syn region stataFunc matchgroup=Function start=/\<_skip(/ end=/)/ contains=stataInt


" Bracket Region (not currently used)
syn region stataBracket start=/{/ end=/{/ contains=ALLBUT,stataInt


"TODO: Errors to Catch


" Highlight Groups
hi def link stataEString       stataString
hi def link stataSlashComment  stataComment
hi def link stataStarComment   stataComment

hi def link stataComment       Comment
hi def link stataCommand       Define
hi def link stataFunc          Function
hi def link stataType          Type
hi def link stataVariable      None
hi def link stataFormat        SpecialChar
hi def link stataString        String
hi def link stataInt           Number
hi def link stataTodo          Todo

let b:current_syntax = "statadct"
