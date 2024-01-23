" Vim syntax file for Stata do, ado, and class files.
" Language: Stata and/or Mata
" Maintainer: Chris Poliquin <chrispoliquin@gmail.com>
" Note: Based on the original by Jeff Pitblado <jpitblado@stata.com>
" Last Change: 2024-01-23
" Version: 0.6.38


" Preamble {{{
" -----------------------------------------------------------------------------
if version < 600
    syntax clear
elseif exists("b:current_syntax")
    finish
endif

syntax case match
" }}}


" Operators {{{
" -----------------------------------------------------------------------------
syn match stataOperator "+"
syn match stataOperator "-"
syn match stataOperator "*"
syn match stataOperator "/"
syn match stataOperator "="
syn match stataOperator "\^"
" logical operators
syn match stataOperator "\!"
syn match stataOperator "\~"
syn match stataOperator "&"
syn match stataOperator "|"
" relational operators
syn match stataOperator ">"
syn match stataOperator "<"
" matrix operators
syn match stataMatrixOperator "#"
syn match stataMatrixOperator "'"
syn match stataMatrixOperator "," contained
syn match stataMatrixOperator "\\"
" }}}


" Variable Types {{{
" -----------------------------------------------------------------------------
syn keyword stataType byte
syn keyword stataType int
syn keyword stataType long
syn keyword stataType float
syn keyword stataType double
syn match   stataType /\<strL\?[0-9]*\>/
" }}}


" Comments {{{
" -----------------------------------------------------------------------------
" words that deserve special highlighting
syn keyword stataTodo TODO FIXME XXX contained

" single line, triple slash continuing line comment comes free
syn region stataStarComment  start=/^\s*\*/ end=/$/ contains=stataComment,stataTodo oneline
syn region stataSlashComment start="\s//"   end=/$/ contains=stataComment,stataTodo oneline
syn region stataSlashComment start="\s///"  end=/$/ contains=stataComment,stataTodo oneline
syn region stataSlashComment start="^//"    end=/$/ contains=stataComment,stataTodo oneline
" multiple line
syn region stataComment start="/\*" end="\*/" contains=stataComment,stataTodo
" }}}


" Macros {{{
" -----------------------------------------------------------------------------
" global macros - simple case
syn match stataGlobal /\$\a\w*/
" global macros - general case
syn region stataGlobal start=/\${/ end=/}/ oneline contains=@stataMacroGroup
" local macros - general case
syn region stataLocal start=/`/ end=/'/ oneline contains=@stataMacroGroup
" }}}


" Statements {{{
" -----------------------------------------------------------------------------
syn keyword stataConditional else if in
syn keyword stataRepeat      foreach
syn keyword stataRepeat      forv[alues]
syn keyword stataRepeat      while
" }}}


" Commands {{{
" -----------------------------------------------------------------------------
syn keyword stataCommand about
syn keyword stataCommand acprplot
syn keyword stataCommand adjust
syn keyword stataCommand adopath
syn keyword stataCommand adoupdate
syn keyword stataCommand append
syn keyword stataCommand areg
syn keyword stataCommand args
syn keyword stataCommand arima
syn keyword stataCommand assert
syn keyword stataCommand avplot
syn keyword stataCommand avplots
syn keyword stataCommand bitest
syn keyword stataCommand bootstrap
syn keyword stataCommand bsqreg
syn keyword stataCommand break
syn keyword stataCommand brier
syn keyword stataCommand by
syn keyword stataCommand bys
syn keyword stataCommand bysort
syn keyword stataCommand cap[ture]
syn keyword stataCommand ci[i] mean[s]
syn keyword stataCommand ci[i] prop[ortions]
syn keyword stataCommand ci[i] var[iances]
syn keyword stataCommand cd
syn keyword stataCommand cf
syn keyword stataCommand chdir
syn keyword stataCommand checksum
syn keyword stataCommand churdle
syn keyword stataCommand class
syn keyword stataCommand classutil
syn keyword stataCommand clear
syn keyword stataCommand cloglog
syn keyword stataCommand codebook
syn keyword stataCommand collapse
syn keyword stataCommand collect
syn keyword stataCommand corr[elate]
syn keyword stataCommand compress
syn keyword stataCommand contrast
syn keyword stataCommand conf[irm]
syn keyword stataCommand conren
syn keyword stataCommand contract
syn keyword stataCommand constraint
syn keyword stataCommand continue
syn keyword stataCommand copy
syn keyword stataCommand cou[nt]
syn keyword stataCommand cpoisson
syn keyword stataCommand cprplot
syn keyword stataCommand cross
syn keyword stataCommand cscript
syn keyword stataCommand cscript_log
syn keyword stataCommand cwf
syn keyword stataCommand #delimit
syn keyword stataCommand decode
syn keyword stataCommand d[escribe]
syn keyword stataCommand destring
syn keyword stataCommand dfuller
syn keyword stataCommand dir
syn keyword stataCommand discard
syn keyword stataCommand di[splay]
syn keyword stataCommand do
syn keyword stataCommand doedit
syn keyword stataCommand drop
syn keyword stataCommand ds
syn keyword stataCommand dstdize
syn match stataCommand '\<duplicates \(r\(eport\)\?\|e\(xamples\)\?\|l\(ist\)\?\|t\(ag\)\?\|drop\)\>'
syn keyword stataCommand edit
syn keyword stataCommand egen[erate]
syn keyword stataCommand encode
syn keyword stataCommand end
syn keyword stataCommand erase
syn keyword stataCommand eret[urn]
syn keyword stataCommand err[or]
syn keyword stataCommand estadd
syn keyword stataCommand etable
syn match stataCommand '\<estat \(ic\|summarize\|vce\|gof\)\>'
syn match stataCommand '\<est\(imates\)\? \(save\|use\|esample\|tab\(le\)\?\|sto\(re\)\?\|res\(tore\)\?\|q\(uery\)\?\|des\(cribe\)\?\|drop\|dir\|clear\)\>'
syn keyword stataCommand excel
syn keyword stataCommand exlogistic
syn keyword stataCommand e[xit]
syn keyword stataCommand expand
syn keyword stataCommand expandcl
syn keyword stataCommand export
syn keyword stataCommand file
syn keyword stataCommand fillin
syn keyword stataCommand findfile
syn keyword stataCommand forecast
syn keyword stataCommand format
syn match stataCommand '\<frame\(s\)\?\( dir\| create\| change\| copy\| put\| post\| drop\| rename\| reset\)\?\>'
syn keyword stataCommand frlink
syn keyword stataCommand frget
syn keyword stataCommand fvrevar
syn match stataCommand '\<fvset \(report\|b\(ase\)\?\|d\(esign\)\?\|clear\)\>'
syn keyword stataCommand g[enerate]
syn keyword stataCommand getmata
syn keyword stataCommand gettoken
syn keyword stataCommand gl[obal]
syn keyword stataCommand glogit
syn keyword stataCommand glm
syn keyword stataCommand gnbreg
syn keyword stataCommand graph
syn keyword stataCommand gsort
syn keyword stataCommand hausman
syn keyword stataCommand help
syn keyword stataCommand heckman
syn keyword stataCommand heckoprobit
syn keyword stataCommand heckpoisson
syn keyword stataCommand heckprobit
syn keyword stataCommand hexdump
syn keyword stataCommand hist[ogram]
syn keyword stataCommand hotelling
syn keyword stataCommand icd9[p]
syn keyword stataCommand icd10
syn keyword stataCommand import
syn keyword stataCommand include
syn keyword stataCommand infile
syn keyword stataCommand infix
syn keyword stataCommand input
syn keyword stataCommand insheet
syn keyword stataCommand inspect
syn keyword stataCommand ipolate
syn keyword stataCommand iqreg
syn keyword stataCommand isid
syn keyword stataCommand ivreg[ress]
syn keyword stataCommand jackknife
syn keyword stataCommand joinby
syn keyword stataCommand keep
syn keyword stataCommand la[bel]
syn keyword stataCommand labelbook
syn keyword stataCommand levelsof
syn keyword stataCommand lincom
syn keyword stataCommand linktest
syn keyword stataCommand list
syn keyword stataCommand loc[al]
syn keyword stataCommand log
syn keyword stataCommand logit
syn keyword stataCommand logistic
syn keyword stataCommand lrtest
syn keyword stataCommand lv
syn keyword stataCommand lvr2plot
syn keyword stataCommand ma[cro]
syn keyword stataCommand manova
syn keyword stataCommand margins[plot]
syn keyword stataCommand mark
syn keyword stataCommand markout
syn keyword stataCommand marksample
syn keyword stataCommand mata
syn keyword stataCommand matname
syn keyword stataCommand mat[rix]
syn keyword stataCommand mecloglog
syn keyword stataCommand median
syn keyword stataCommand melogit
syn keyword stataCommand meglm
syn keyword stataCommand memory
syn keyword stataCommand menbreg
syn keyword stataCommand menl
syn keyword stataCommand meologit
syn keyword stataCommand meoprobit
syn keyword stataCommand mepoisson
syn keyword stataCommand meprobit
syn keyword stataCommand merge
syn keyword stataCommand mestreg
syn keyword stataCommand meqrpoisson
syn keyword stataCommand meqrlogit
syn keyword stataCommand misstable
syn keyword stataCommand mixed
syn keyword stataCommand mkdir
syn keyword stataCommand mkmat
syn keyword stataCommand mkspline
syn keyword stataCommand more
syn keyword stataCommand mvdecode
syn keyword stataCommand mvencode
syn keyword stataCommand mvtest m[eans]
syn keyword stataCommand nbreg
syn keyword stataCommand net
syn keyword stataCommand nlcom
syn keyword stataCommand nobreak
syn keyword stataCommand nois[ily]
syn keyword stataCommand note[s]
syn keyword stataCommand numlabel
syn keyword stataCommand numlist
syn keyword stataCommand odbc
syn keyword stataCommand order
syn keyword stataCommand outfile
syn keyword stataCommand outsheet
syn keyword stataCommand _parse
syn keyword stataCommand pause
syn keyword stataCommand pctile
syn keyword stataCommand _pctile
syn keyword stataCommand permute
syn keyword stataCommand plugin
syn keyword stataCommand poisson
syn keyword stataCommand post
syn keyword stataCommand postclose
syn keyword stataCommand postfile
syn keyword stataCommand predict[nl]
syn keyword stataCommand preserve
syn keyword stataCommand probit
syn keyword stataCommand prtest
syn keyword stataCommand putexcel
syn keyword stataCommand putmata
syn keyword stataCommand pwcompare
syn keyword stataCommand pwd
syn keyword stataCommand print
syn keyword stataCommand printer
syn keyword stataCommand profiler
syn keyword stataCommand pr[ogram]
syn keyword stataCommand proportion
syn keyword stataCommand pwcorr
syn keyword stataCommand pwf
syn keyword stataCommand _qreg
syn keyword stataCommand _qregwls
syn keyword stataCommand qreg
syn keyword stataCommand qreg_c
syn keyword stataCommand quadchk
syn keyword stataCommand q[uery]
syn keyword stataCommand qui[etly]
syn keyword stataCommand _rmcoll
syn keyword stataCommand _rmdcoll
syn keyword stataCommand _rmcollright
syn keyword stataCommand _robust
syn keyword stataCommand rcof
syn keyword stataCommand recast
syn keyword stataCommand recode
syn keyword stataCommand reg[ress]
syn keyword stataCommand ren[ame]
syn keyword stataCommand repeat
syn keyword stataCommand replace
syn keyword stataCommand reshape
syn keyword stataCommand restore
syn keyword stataCommand ret[urn]
syn keyword stataCommand rmdir
syn keyword stataCommand rolling
syn keyword stataCommand roc
syn keyword stataCommand rreg
syn keyword stataCommand run
syn keyword stataCommand rvfplot
syn keyword stataCommand rvpplot
syn keyword stataCommand sample
syn keyword stataCommand save
syn keyword stataCommand saveold
syn keyword stataCommand sca[lar]
syn keyword stataCommand search
syn keyword stataCommand separate
syn keyword stataCommand serset
syn keyword stataCommand set
syn keyword stataCommand shell
syn keyword stataCommand simulate
syn keyword stataCommand sleep
syn keyword stataCommand sort
syn keyword stataCommand split
syn keyword stataCommand sqreg
syn keyword stataCommand sret[urn]
syn keyword stataCommand ssc
syn keyword stataCommand stack
syn keyword stataCommand statsby
syn keyword stataCommand stem
syn keyword stataCommand stset
syn keyword stataCommand su[mmarize]
syn keyword stataCommand suest
syn keyword stataCommand svmat
syn keyword stataCommand syntax
syn keyword stataCommand sysdescribe
syn keyword stataCommand sysdir
syn keyword stataCommand sysuse
syn keyword stataCommand tabdisp
syn keyword stataCommand table
syn keyword stataCommand tab[ulate]
syn keyword stataCommand tab1
syn keyword stataCommand tab2
syn keyword stataCommand tabi
syn keyword stataCommand tabstat
syn keyword stataCommand tempfile
syn keyword stataCommand tempname
syn keyword stataCommand tempvar
syn keyword stataCommand test
syn keyword stataCommand testnl
syn match stataCommand '\<timer \(clear\|on\|off\|list\)\>'
syn keyword stataCommand tnbreg
syn keyword stataCommand token[ize]
syn keyword stataCommand tostring
syn keyword stataCommand tpoisson
syn keyword stataCommand translate
syn keyword stataCommand tsline
syn keyword stataCommand tsfill
syn keyword stataCommand tsset
syn keyword stataCommand ttest
syn keyword stataCommand twoway
syn keyword stataCommand type
syn keyword stataCommand unab
syn keyword stataCommand unabcmd
syn match stataCommand '\<unicode analyze\>'
syn match stataCommand '\<unicode loc\(ale\)\? list\>'
syn match stataCommand '\<unicode coll\(ator\)\? list\>'
syn match stataCommand '\<unicode conv\(ertfile\)\?\>'
syn match stataCommand '\<unicode enc\(oding\)\? \(set\|alias\|list\)\>'
syn match stataCommand '\<unicode erasebackups\>'
syn match stataCommand '\<unicode retr\(anslate\)\?\>'
syn match stataCommand '\<unicode restore\>'
syn match stataCommand '\<unicode tr\(anslate\)\?\>'
syn match stataCommand '\<unicode ui\(package\)\? list\>'
syn keyword stataCommand unzipfile
syn keyword stataCommand update
syn keyword stataCommand use
syn keyword stataCommand uselabel
syn keyword stataCommand using
syn keyword stataCommand vers[ion]
syn keyword stataCommand view
syn keyword stataCommand viewsource
syn keyword stataCommand webdescribe
syn keyword stataCommand webseek
syn keyword stataCommand webuse
syn keyword stataCommand which
syn keyword stataCommand who
syn keyword stataCommand window
syn keyword stataCommand xtdes[cribe]
syn keyword stataCommand xi
syn keyword stataCommand xtabond
syn keyword stataCommand xtcloglog
syn keyword stataCommand xtdata
syn keyword stataCommand xtdpd
syn keyword stataCommand xtdpdsys
syn keyword stataCommand xtfrontier
syn keyword stataCommand xtgls
syn keyword stataCommand xtgee
syn keyword stataCommand xtile
syn keyword stataCommand xtivreg
syn keyword stataCommand xtline
syn keyword stataCommand xtlogit
syn keyword stataCommand xthtaylor
syn keyword stataCommand xtintreg
syn keyword stataCommand xtheckman
syn keyword stataCommand xtmelogit
syn keyword stataCommand xtmepoisson
syn keyword stataCommand xtnbreg
syn keyword stataCommand xtologit
syn keyword stataCommand xtoprobit
syn keyword stataCommand xtpcse
syn keyword stataCommand xtpoisson
syn keyword stataCommand xtprobit
syn keyword stataCommand xtrc
syn keyword stataCommand xtreg
syn keyword stataCommand xtregar
syn keyword stataCommand xtset
syn keyword stataCommand xtstreg
syn keyword stataCommand xtsum
syn keyword stataCommand xttab
syn keyword stataCommand xttobit
syn keyword stataCommand xtunitroot
syn keyword stataCommand zinb
syn keyword stataCommand zip
syn keyword stataCommand zipfile
" }}}


" Popular user-written commands from ssc or elsewhere {{{
" -----------------------------------------------------------------------------
syn keyword stataCommand asdoc
syn keyword stataCommand astile
syn keyword stataCommand collin
syn keyword stataCommand distinct
syn keyword stataCommand eglist
syn keyword stataCommand estout
syn keyword stataCommand estpost
syn keyword stataCommand eststo
syn keyword stataCommand esttab
syn keyword stataCommand fasterxtile
syn keyword stataCommand fcollapse
syn keyword stataCommand fegen
syn keyword stataCommand findname
syn keyword stataCommand fisid
syn keyword stataCommand fmerge
syn keyword stataCommand flevelsof
syn keyword stataCommand grc1leg
syn keyword stataCommand gcollapse
syn keyword stataCommand greshape
syn keyword stataCommand gegen
syn keyword stataCommand gcontract
syn keyword stataCommand gisid
syn keyword stataCommand givregress
syn keyword stataCommand glevelsof
syn keyword stataCommand gduplicates
syn keyword stataCommand gquantiles
syn keyword stataCommand gregress
syn keyword stataCommand gstats
syn keyword stataCommand gunique
syn keyword stataCommand gdistinct
syn keyword stataCommand gtop
syn keyword stataCommand hashsort
syn keyword stataCommand hdfe
syn keyword stataCommand ivreg2
syn keyword stataCommand ivreg28
syn keyword stataCommand ivreg29
syn keyword stataCommand ivreg210
syn keyword stataCommand ivreghdfe
syn keyword stataCommand jarowinkler
syn keyword stataCommand keeporder
syn keyword stataCommand kountry
syn keyword stataCommand labcd
syn keyword stataCommand labcopy
syn keyword stataCommand labdel
syn keyword stataCommand labdtch
syn keyword stataCommand lablog
syn keyword stataCommand labmap
syn keyword stataCommand labmask
syn keyword stataCommand labnoeq
syn keyword stataCommand labvalch
syn keyword stataCommand labvarch
syn keyword stataCommand labvalclone
syn keyword stataCommand labvalcombine
syn keyword stataCommand mdesc
syn keyword stataCommand mipolate
syn keyword stataCommand missings
syn keyword stataCommand outreg[2]
syn keyword stataCommand ppmlhdfe
syn keyword stataCommand psmatch2
syn keyword stataCommand reclink
syn keyword stataCommand reghdfe
syn keyword stataCommand regsave
syn keyword stataCommand rsource
syn keyword stataCommand sumtable
syn keyword stataCommand stan
syn keyword stataCommand svmat2
syn keyword stataCommand sxpose
syn keyword stataCommand tabmiss
syn keyword stataCommand tabout
syn keyword stataCommand tabstatmat
syn keyword stataCommand winsor
syn keyword stataCommand winsor2
syn keyword stataCommand unique

" user written egen functions (egenmisc and egenmore packages) {{{
syn region stataFunc matchgroup=Function start=/\<anycount(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<fastwpctile(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<fastxtile(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<nacorr(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<nacov(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<namean(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<nasd(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<nasum(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<navar(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<pick(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<axis(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<clsst(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<egroup(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<group2(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<mlabvpos(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<base(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<decimal(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<incss(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<iso3166(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<msub(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<noccur(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<nss(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<ntos(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<nwords(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<repeat(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<sieve(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<ston(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<truncdig(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<wordof(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<bom(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<bomd(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<dayofyear(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<dhms(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<elap(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<elap2(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<eom(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<eomd(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<ewma(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<filter(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<filter7(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<foy(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<hmm(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<hmmss(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<hms(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<minutes(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<ncyear(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<record(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<seconds(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<tod(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<adjl(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<adju(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<corr(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<d2(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<density(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<gmean(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<hmean(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<nmiss(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<nvals(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<outside(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<ridit(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<semean(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<sumoth(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<var(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<wpctile(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<wtfreq(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<xtile(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<first(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<ifirst(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<ilast(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<lastnm(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<mixnorm(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<rndint(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<rndsub(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<rall(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<rany(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<rcount(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<rowmedian(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<rownvals(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<rowsvals(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<rsum2(/ end=/)/ contains=@stataFuncGroup
" }}}
" }}}


" Strings {{{
" -----------------------------------------------------------------------------
syn match stataQuote /"/
syn region stataEString matchgroup=Nothing start=/`"/ end=/"'/ oneline contains=@stataMacroGroup,stataQuote,stataString,stataEString
syn region stataString start=/"/ end=/"/ oneline contains=@stataMacroGroup
" }}}


" Clusters {{{
" -----------------------------------------------------------------------------
syn cluster stataFuncGroup contains=@stataMacroGroup,stataFunc,stataString,stataEstring,stataParen,stataBracket,stataComment,stataNumber,stataFloat,stataBinNumber,stataHexNumber,stataOctNumber
syn cluster stataMacroGroup contains=stataGlobal,stataLocal
syn cluster stataParenGroup contains=stataParenError,stataBracketError,stataBraceError,stataSpecial
" }}}


" Built-in Functions {{{
" -----------------------------------------------------------------------------
" egen functions {{{
" TODO: not all functions accept everything in stataFuncGroup as arguments
syn region stataFunc matchgroup=Function start=/\<anycount(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<anymatch(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<anyvalue(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<concat(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<count(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<cut(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<diff(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<ends(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<fill(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<group(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<iqr(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<kurt(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<mad(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<max(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<mdev(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<mean(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<median(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<min(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<mode(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<mtr(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<pc(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<pctile(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<rank(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<rowfirst(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<rowlast(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<rowmax(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<rowmean(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<rowmedian(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<rowmin(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<rowmiss(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<rownonmiss(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<rowpctile(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<rowsd(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<rowtotal(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<sd(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<seq(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<skew(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<std(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<tag(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<total(/ end=/)/ contains=@stataFuncGroup
" }}}

" math {{{
syn region stataFunc matchgroup=Function start=/\<abs(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<acos(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<acosh(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<asin(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<asinh(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<atan(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<atan2(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<atanh(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<ceil(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<cloglog(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<comb(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<cos(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<cosh(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<digamma(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<exp(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<expm1(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<floor(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<int(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<invcloglog(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<invlogit(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<ln(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<ln1m(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<ln1p(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<lnfactorial(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<lngamma(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<log(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<log10(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<logit(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<max(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<min(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<mod(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<reldif(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<round(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<sign(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<sin(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<sinh(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<sqrt(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<sum(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<tan(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<tanh(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<trigamma(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<trunc(/ end=/)/ contains=@stataFuncGroup
" }}}

" probability distriubtions and density functions {{{
syn region stataFunc matchgroup=Function start=/\<betaden(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<Binomial(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<binorm(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<binormal(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<chi2(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<chi2den(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<chi2tail(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<dgammapda(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<dgammapdada(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<dgammapdadx(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<dgammapdx(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<dgammapdxdx(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<dunnettprob(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<F(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<Fden(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<Ftail(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<gammaden(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<gammap(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<gammaptail(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<hypergeometric(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<hypergeometricp(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<ibeta(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<igaussian(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<igaussianden(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<igaussiantail(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<invbinomial(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<invbinomialtail(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<invnbinomial(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<invnbinomialtail(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<invchi2(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<invchi2tail(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<invdunnettprob(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<invF(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<invFtail(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<invgammap(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<invgammaptail(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<invibeta(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<invigaussian(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<invigaussiantail(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<invnchi2(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<invFtail(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<invibeta(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<invnormal(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<invnormaltail(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<invpoisson(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<invpoissontail(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<invt(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<invnttail(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<invttail(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<invtukeyprob(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<lnigaussianden(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<lnnormal(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<lnnormalden(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<nbetaden(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<nbinomial(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<nbinomialp(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<nbinomialtail(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<nchi2(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<nFden(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<nFtail(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<nibeta(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<normal(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<normalden(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<normden(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<npnchi2(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<npnt(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<nt(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<ntden(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<nttail(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<poisson(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<poissonp(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<poissontail(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<rigaussian(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<t(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<tden(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<ttail(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<tukeyprob(/ end=/)/ contains=@stataFuncGroup
" }}}

" random numbers {{{
syn region stataFunc matchgroup=Function start=/\<runiform(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<runiformint(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<rbeta(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<rbinomial(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<rchi2(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<rexponential(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<rgamma(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<rhypergeometric(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<rlogistic(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<rnbinomial(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<rnormal(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<rpoisson(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<rt(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<rweibull(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<rweibullph(/ end=/)/ contains=@stataFuncGroup
" }}}

" strings {{{
syn region stataFunc matchgroup=Function start=/\<abbrev(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<char(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<collatorlocale(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<collatorversion(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<indexnot(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<itrim(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<length(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<lower(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<ltrim(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<plural(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<proper(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<real(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<regexm(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<regexr(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<regexs(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<reverse(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<rtrim(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<soundex(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<soundex_nara(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<strcat(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<strdup(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<string(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<stritrim(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<strlen(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<strlower(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<strltrim(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<strmatch(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<strofreal(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<strpos(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<strproper(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<strreverse(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<strrtrim(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<strtoname(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<strtrim(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<strupper(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<subinstr(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<subinword(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<substr(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<tobytes(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<trim(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<uchar(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<udstrlen(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<udsubstr(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<uisdigit(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<uisletter(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<ustrcompare(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<ustrcompareex(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<ustrpos(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<ustrrpos(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<ustrfix(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<ustrfrom(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<ustrinvalidcnt(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<ustrleft(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<ustrlen(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<ustrlower(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<ustrltrim(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<ustrnormalize(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<ustrregexm(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<ustrregexra(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<ustrregexrf(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<ustrregexs(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<ustrreverse(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<ustrright(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<ustrtrim(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<ustrsortkey(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<ustrsortkeyex(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<ustrtitle(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<ustrto(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<ustrtohex(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<ustrtoname(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<ustrtrim(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<ustrunescape(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<ustrupper(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<ustrword(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<ustrwordcount(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<usubinstr(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<usubstr(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<upper(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<word(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<wordbreaklocale(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<wordcount(/ end=/)/ contains=@stataFuncGroup
" }}}

" Frames {{{
" -----------------------------------------------------------------------------
syn region stataFunc matchgroup=Function start=/\<frval(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<_frval(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<st_framecurrent(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<st_framedir(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<st_framecreate(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<_st_framecreate(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<st_framecurrent(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<_st_framecurrent(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<st_framerename(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<_st_framerename(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<st_framedrop(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<_st_framedrop(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<st_framedropabc(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<st_framereset(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<st_framecopy(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<_st_framecopy(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<st_frameexists(/ end=/)/ contains=@stataFuncGroup
" }}}

" programming {{{
syn region stataFunc matchgroup=Function start=/\<autocode(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<byteorder(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<c(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<_caller(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<chop(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<classname(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<clip(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<cond(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<e(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<epsdouble(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<epsfloat(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<fileexists(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<fileread(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<filereaderror(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<filewrite(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<float(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<has_eprop(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<has_eprop(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<inlist(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<inrange(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<irecode(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<matrix(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<maxbyte(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<maxdouble(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<maxfloat(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<maxint(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<maxlong(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<mi(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<minbyte(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<mindouble(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<minfloat(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<minint(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<minlong(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<missing(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<r(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<recode(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<replay(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<return(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<s(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<scalar(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<smallestdouble(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<structname(/ end=/)/ contains=@stataFuncGroup
" }}}

" date {{{
syn region stataFunc matchgroup=Function start=/\<bofd(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<Cdhms(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<Chms(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<Clock(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<clock(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<Cmdyhms(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<Cofc(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<cofC(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<Cofd(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<cofd(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<daily(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<date(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<day(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<dhms(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<dofb(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<dofC(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<dofc(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<dofh(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<dofm(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<dofq(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<dofw(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<dofy(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<dow(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<doy(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<halfyear(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<halfyearly(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<hh(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<hhC(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<hms(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<hofd(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<hours(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<mdy(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<mdyhms(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<minutes(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<mm(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<mmC(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<mofd(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<month(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<monthly(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<msofhours(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<msofminutes(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<msofseconds(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<qofd(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<quarter(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<quarterly(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<seconds(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<ss(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<ssC(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<tC(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<tc(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<td(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<th(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<tm(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<tq(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<tw(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<week(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<weekly(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<wofd(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<year(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<yearly(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<yh(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<ym(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<yofd(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<yq(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<yw(/ end=/)/ contains=@stataFuncGroup
" }}}

" time-series {{{
syn region stataFunc matchgroup=Function start=/\<tin(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<twithin(/ end=/)/ contains=@stataFuncGroup
" }}}

" matrix {{{
syn region stataFunc matchgroup=Function start=/\<colnumb(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<colsof(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<det(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<diag0cnt(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<el(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<issymmetric(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<matmissing(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<mreldif(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<rownumb(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<rowsof(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<trace(/ end=/)/ contains=@stataFuncGroup

syn region stataFunc matchgroup=Function start=/\<cholesky(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<corr(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<diag(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<get(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<hadamard(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<I(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<inv(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<invsym(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<J(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<matuniform(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<nullmat(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<sweep(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<vec(/ end=/)/ contains=@stataFuncGroup
syn region stataFunc matchgroup=Function start=/\<vecdiag(/ end=/)/ contains=@stataFuncGroup
" }}}
" End Build-in Functions }}}


" Numbers {{{
" -----------------------------------------------------------------------------
" (patterns from github.com/zizhongyan/stata-vim-syntax)
syn match stataNumber    "\<\d\>"        display
syn match stataNumber    "\<[0-9]\d\+\>" display
syn match stataNumber    "\<\d\+[jJ]\>"  display
syn match stataFloat     "\.\d\+\%([eE][+-]\=\d\+\)\=[jJ]\=\>"      display
syn match stataFloat     "\<\d\+[eE][+-]\=\d\+[jJ]\=\>"             display
syn match stataFloat     "\<\d\+\.\d*\%([eE][+-]\=\d\+\)\=[jJ]\=\>" display
syn match stataBinNumber "\<0[bB][01]\+[lL]\=\>" display
syn match stataBinNumber "\<0[bB][01]\+\>"       display
syn match stataHexNumber "\<0[xX]\x\+[lL]\=\>"   display
syn match stataHexNumber "\<0[xX]\x\+\>"         display
syn match stataOctNumber "\<0[oO]\o\+[lL]\=\>"   display
syn match stataOctNumber "\<0[oO]\o\+\>"         display
" }}}


" Formats {{{
" -----------------------------------------------------------------------------
" numeric
syn match  stataFormat /%-\=\d\+\.\d\+[efg]c\=/
" hex format
syn match  stataFormat /%-\=21x/
" string
syn match  stataFormat /%\(\|-\|\~\)\d\+s/
" }}}


" Errors {{{
" -----------------------------------------------------------------------------
" taken from $VIMRUNTIME/syntax/c.vim
" catch errors caused by wrong parenthesis, braces and brackets
syn region stataParen   transparent start=/(/  end=/)/ contains=ALLBUT,@stataParenGroup,stataErrInBracket,stataErrInBrace
syn region stataBracket transparent start=/\[/ end=/]/ contains=ALLBUT,@stataParenGroup,stataErrInParen,stataErrInBrace
syn region stataBrace   transparent start=/{/  end=/}/ contains=ALLBUT,@stataParenGroup,stataErrInParen,stataErrInBracket
syn match  stataParenError    /[\])}]/
syn match  stataBracketError  /]/
syn match  stataBraceError    /}/
syn match  stataErrInParen    contained /[\]}]/
syn match  stataErrInBracket  contained /[)}]/
syn match  stataErrInBrace    contained /[)\]]/
" }}}


" Highlight Groups {{{
" -----------------------------------------------------------------------------
hi def link stataBraceError     stataError
hi def link stataBracketError   stataError
hi def link stataErrInBrace     stataError
hi def link stataErrInBracket   stataError
hi def link stataErrInParen     stataError
hi def link stataEString        stataString
hi def link stataFormat         stataSpecial
hi def link stataGlobal         stataMacro
hi def link stataLocal          stataMacro
hi def link stataParenError     stataError
hi def link stataSlashComment   stataComment
hi def link stataStarComment    stataComment
hi def link stataMatrixOperator stataOperator
hi def link stataHexNumber      stataNumber
hi def link stataOctNumber      stataNumber
hi def link stataBinNumber      stataNumber

hi def link stataCommand        Define
hi def link stataComment        Comment
hi def link stataConditional    Conditional
hi def link stataError          Error
hi def link stataFunc           None
hi def link stataMacro          Identifier
hi def link stataRepeat         Repeat
hi def link stataOperator       Operator
hi def link stataType           Type
hi def link stataSpecial        Constant
hi def link stataString         String
hi def link stataNumber         Number
hi def link stataFloat          Float
hi def link stataTodo           Todo
" }}}


let b:current_syntax = "stata"

" vim: foldmethod=marker
