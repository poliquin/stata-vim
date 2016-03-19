" syntax/smcl.vim -- Vim syntax file for smcl files.
" Language:	SMCL -- Stata Markup and Control Language
" Maintainer:	Jeff Pitblado <jpitblado at stata.com>
" Last Change:	10aug2014
" Version:	2.0.0
" Note:		updated for Stata 13

if version < 600
	syntax clear
elseif exists("b:current_syntax")
	finish
endif

syntax case match

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" c-class values '{ccl <name>}' where <name> taken from -help creturn-

" System values
syn keyword smclCCLword current_date		contained
syn keyword smclCCLword current_time		contained
syn keyword smclCCLword rmsg_time		contained
syn keyword smclCCLword stata_version		contained
syn keyword smclCCLword version			contained
syn keyword smclCCLword born_date		contained
syn keyword smclCCLword flavor			contained
syn keyword smclCCLword bit			contained
syn keyword smclCCLword SE			contained
syn keyword smclCCLword MP			contained
syn keyword smclCCLword processors		contained
syn keyword smclCCLword processors_lic		contained
syn keyword smclCCLword mode			contained
syn keyword smclCCLword console			contained
syn keyword smclCCLword os			contained
syn keyword smclCCLword osdtl			contained
syn keyword smclCCLword hostname		contained
syn keyword smclCCLword machine_type		contained
syn keyword smclCCLword byteorder		contained
syn keyword smclCCLword username		contained

" Directories and paths
syn keyword smclCCLword sysdir_stata		contained
syn keyword smclCCLword sysdir_base		contained
syn keyword smclCCLword sysdir_site		contained
syn keyword smclCCLword sysdir_plus		contained
syn keyword smclCCLword sysdir_personal		contained
syn keyword smclCCLword sysdir_oldplace		contained
syn keyword smclCCLword tmpdir			contained
syn keyword smclCCLword adopath			contained
syn keyword smclCCLword pwd			contained
syn keyword smclCCLword dirsep			contained

" System limits
syn keyword smclCCLword max_N_theory		contained
syn keyword smclCCLword max_k_theory		contained
syn keyword smclCCLword max_width_theory	contained
syn keyword smclCCLword max_matsize		contained
syn keyword smclCCLword min_matsize		contained
syn keyword smclCCLword max_macrolen		contained
syn keyword smclCCLword macrolen		contained
syn keyword smclCCLword max_cmdlen		contained
syn keyword smclCCLword cmdlen			contained
syn keyword smclCCLword namelen			contained
syn keyword smclCCLword eqlen			contained

" Numerical and string limits
syn keyword smclCCLword mindouble		contained
syn keyword smclCCLword maxdouble		contained
syn keyword smclCCLword epsdouble		contained
syn keyword smclCCLword smallestdouble		contained
syn keyword smclCCLword minfloat		contained
syn keyword smclCCLword maxfloat		contained
syn keyword smclCCLword epsfloat		contained
syn keyword smclCCLword minlong			contained
syn keyword smclCCLword maxlong			contained
syn keyword smclCCLword minint			contained
syn keyword smclCCLword maxint			contained
syn keyword smclCCLword minbyte			contained
syn keyword smclCCLword maxbyte			contained
syn keyword smclCCLword maxstrvarlen		contained
syn keyword smclCCLword maxstrlvarlen		contained
syn keyword smclCCLword maxvlabellen		contained

" Current dataset
syn keyword smclCCLword N			contained
syn keyword smclCCLword k			contained
syn keyword smclCCLword width			contained
syn keyword smclCCLword changed			contained
syn keyword smclCCLword filename		contained
syn keyword smclCCLword filedate		contained

" Memory settings
syn keyword smclCCLword memory			contained
syn keyword smclCCLword maxvar			contained
syn keyword smclCCLword matsize			contained
syn keyword smclCCLword niceness		contained
syn keyword smclCCLword min_memory		contained
syn keyword smclCCLword max_memory		contained
syn keyword smclCCLword segmentsize		contained

" Output settings
syn keyword smclCCLword more			contained
syn keyword smclCCLword rmsg			contained
syn keyword smclCCLword dp			contained
syn keyword smclCCLword linesize		contained
syn keyword smclCCLword pagesize		contained
syn keyword smclCCLword logtype			contained
syn keyword smclCCLword noisily			contained
syn keyword smclCCLword charset			contained
syn keyword smclCCLword eolchar			contained
syn keyword smclCCLword notifyuser		contained
syn keyword smclCCLword playsnd			contained
syn keyword smclCCLword include_bitmap		contained
syn keyword smclCCLword level			contained
syn keyword smclCCLword showbaselevels		contained
syn keyword smclCCLword showemptycells		contained
syn keyword smclCCLword showomitted		contained
syn keyword smclCCLword fvlabel			contained
syn keyword smclCCLword fvwrap			contained
syn keyword smclCCLword fvwrapon		contained
syn keyword smclCCLword lstretch		contained
syn keyword smclCCLword cformat			contained
syn keyword smclCCLword sformat			contained
syn keyword smclCCLword pformat			contained
syn keyword smclCCLword coeftabresults		contained

" Interface settings
syn keyword smclCCLword dockable		contained
syn keyword smclCCLword dockingguides		contained
syn keyword smclCCLword locksplitters		contained
syn keyword smclCCLword pinnable		contained
syn keyword smclCCLword doublebuffer		contained
syn keyword smclCCLword reventries		contained
syn keyword smclCCLword fastscroll		contained
syn keyword smclCCLword revkeyboard		contained
syn keyword smclCCLword varkeyboard		contained
syn keyword smclCCLword smoothfonts		contained
syn keyword smclCCLword linegap			contained
syn keyword smclCCLword scrollbufsize		contained
syn keyword smclCCLword maxdb			contained

" Graphics settings
syn keyword smclCCLword graphics		contained
syn keyword smclCCLword autotabgraphs		contained
syn keyword smclCCLword scheme			contained
syn keyword smclCCLword printcolor		contained
syn keyword smclCCLword copycolor		contained

" Efficienty settings
syn keyword smclCCLword adosize			contained

" Network settings
syn keyword smclCCLword checksum		contained
syn keyword smclCCLword timeout1		contained
syn keyword smclCCLword timeout2		contained
syn keyword smclCCLword httpproxy		contained
syn keyword smclCCLword httpproxyhost		contained
syn keyword smclCCLword httpproxyport		contained
syn keyword smclCCLword httpproxyauth		contained
syn keyword smclCCLword httpproxyuser		contained
syn keyword smclCCLword httpproxypw		contained

" Update settings
syn keyword smclCCLword update_query		contained
syn keyword smclCCLword update_interval		contained
syn keyword smclCCLword update_prompt		contained

" Trace (program debugging) settings
syn keyword smclCCLword trace			contained
syn keyword smclCCLword tracedepth		contained
syn keyword smclCCLword tracesep		contained
syn keyword smclCCLword traceindent		contained
syn keyword smclCCLword traceexpand		contained
syn keyword smclCCLword tracenumber		contained
syn keyword smclCCLword tracehilite		contained

" Mata settings
syn keyword smclCCLword matastrict		contained
syn keyword smclCCLword matalnum		contained
syn keyword smclCCLword mataoptimize		contained
syn keyword smclCCLword matafavor		contained
syn keyword smclCCLword matacache		contained
syn keyword smclCCLword matalibs		contained
syn keyword smclCCLword matamofirst		contained

" Other settings
syn keyword smclCCLword type			contained
syn keyword smclCCLword maxiter			contained
syn keyword smclCCLword searchdefault		contained
syn keyword smclCCLword seed			contained
syn keyword smclCCLword version_rng		contained
syn keyword smclCCLword varabbrev		contained
syn keyword smclCCLword emptycells		contained
syn keyword smclCCLword haverdir		contained
syn keyword smclCCLword odbcmgr			contained

" Other
syn keyword smclCCLword pi			contained
syn keyword smclCCLword alpha			contained
syn keyword smclCCLword ALPHA			contained
syn keyword smclCCLword Mons			contained
syn keyword smclCCLword Months			contained
syn keyword smclCCLword Wdays			contained
syn keyword smclCCLword Weekdays		contained
syn keyword smclCCLword rc			contained

" Directive for the constant and current-value class
syn region smclCCL start=/{ccl / end=/}/ oneline contains=smclCCLword

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" The order of the following syntax definitions is roughly that of the
" documentation for smcl in Stata, -help smcl-.

" Help file preprocessor directive for substituting repeated material
syn match smclFormat /INCLUDE help .\+/

" Format directives for use in line and paragraph modes
syn match smclFormat /{smcl}/
syn match smclFormat /{sf\(\|:[^}]\+\)}/
syn match smclFormat /{it\(\|:[^}]\+\)}/
syn match smclFormat /{bf\(\|:[^}]\+\)}/
syn match smclFormat /{inp\(\|:[^}]\+\)}/
syn match smclFormat /{input\(\|:[^}]\+\)}/
syn match smclFormat /{err\(\|:[^}]\+\)}/
syn match smclFormat /{error\(\|:[^}]\+\)}/
syn match smclFormat /{res\(\|:[^}]\+\)}/
syn match smclFormat /{result\(\|:[^}]\+\)}/
syn match smclFormat /{txt\(\|:[^}]\+\)}/
syn match smclFormat /{text\(\|:[^}]\+\)}/
syn match smclFormat /{cmd\(\|:[^}]\+\)}/
syn match smclFormat /{cmdab:[^:}]\+:[^:}()]*\(\|:\|:(\|:()\)}/
syn match smclFormat /{opt \w\+\(\|:\w\+\)\(\|([^)}]*)\)}/
syn match smclFormat /{opth \w*\(\|:\w\+\)(\w*)}/
syn match smclFormat /{opth "\w\+\((\w\+:[^)}]\+)\)"}/
syn match smclFormat /{opth \w\+:\w\+(\w\+:[^)}]\+)}/
syn match smclFormat /{hi\(\|:[^}]\+\)}/
syn match smclFormat /{hilite\(\|:[^}]\+\)}/
syn match smclFormat /{ul \(on\|off\)}/
syn match smclFormat /{ul:[^}]\+}/
syn match smclFormat /{hline\(\| \d\+\| -\d\+\|:[^}]\+\)}/
syn match smclFormat /{\.-}/
syn match smclFormat /{dup \d\+:[^}]\+}/
syn match smclFormat /{c [^}]\+}/
syn match smclFormat /{char [^}]\+}/
syn match smclFormat /{reset}/

" Link directive for use in line and paragraph modes
syn match smclLink /{help [^}]\+}/
syn match smclLink /{helpb [^}]\+}/
syn match smclLink /{manhelp [^}]\+}/
syn match smclLink /{manhelpi [^}]\+}/
syn match smclLink /{help_d:[^}]\+}/
syn match smclLink /{newvar\(\|:[^}]\+\)}/
syn match smclLink /{var\(\|:[^}]\+\)}/
syn match smclLink /{varname\(\|:[^}]\+\)}/
syn match smclLink /{vars\(\|:[^}]\+\)}/
syn match smclLink /{varlist\(\|:[^}]\+\)}/
syn match smclLink /{depvar\(\|:[^}]\+\)}/
syn match smclLink /{depvars\(\|:[^}]\+\)}/
syn match smclLink /{depvarlist\(\|:[^}]\+\)}/
syn match smclLink /{indepvars\(\|:[^}]\+\)}/
syn match smclLink /{ifin}/
syn match smclLink /{weight}/
syn match smclLink /{dtype}/
syn match smclLink /{search [^}]\+}/
syn match smclLink /{search_d:[^}]\+}/
syn match smclLink /{dialog [^}]\+}/
syn match smclLink /{browse [^}]\+}/
syn match smclLink /{view [^}]\+}/
syn match smclLink /{view_d:[^}]\+}/
syn match smclLink /{manpage [^}]\+}/
syn match smclLink /{mansection [^}]\+}/
syn match smclLink /{manlink [^}]\+}/
syn match smclLink /{manlinki [^}]\+}/
syn match smclLink /{news:[^}]\+}/
syn match smclLink /{net [^}]\+}/
syn match smclLink /{net_d:[^}]\+}/
syn match smclLink /{netfrom_d:[^}]\+}/
syn match smclLink /{ado [^}]\+}/
syn match smclLink /{ado_d:[^}]\+}/
syn match smclLink /{update [^}]\+}/
syn match smclLink /{update_d:[^}]\+}/
syn match smclLink /{back:[^}]\+}/
syn match smclLink /{clearmore:[^}]\+}/
syn match smclLink /{stata [^}]\+}/
syn match smclLink /{matacmd [^}]\+}/

" Formatting directives for use in line mode
syn match smclFormat /{title:[^}]\+}/
syn match smclFormat /{center:[^}]\+}/
syn match smclFormat /{centre:[^}]\+}/
syn match smclFormat /{center \d\+:[^}]\+}/
syn match smclFormat /{centre \d\+:[^}]\+}/
syn match smclFormat /{rcenter:[^}]\+}/
syn match smclFormat /{rcentre:[^}]\+}/
syn match smclFormat /{rcenter \d\+:[^}]\+}/
syn match smclFormat /{rcentre \d\+:[^}]\+}/
syn match smclFormat /{right:[^}]\+}/
syn match smclFormat /{lalign \d\+:[^}]\+}/
syn match smclFormat /{ralign \d\+:[^}]\+}/
syn match smclFormat /{dlgtab\s*\(\|\d\+\|\d\+\s\+\d\+\):[^}]\+}/
syn match smclFormat /{\.\.\.}/
syn match smclFormat /{col \d\+}/
syn match smclFormat /{space \d\+}/
syn match smclFormat /{tab}/

" Formatting directives for use in paragraph mode
syn match smclFormat /{p}/
syn match smclFormat /{p \d\+}/
syn match smclFormat /{p \d\+ \d\+}/
syn match smclFormat /{p \d\+ \d\+ \d\+}/
syn match smclFormat /{pstd}/
syn match smclFormat /{psee}/
syn match smclFormat /{phang\(\|2\|3\)}/
syn match smclFormat /{pmore\(\|2\|3\)}/
syn match smclFormat /{pin\(\|2\|3\)}/
syn match smclFormat /{p_end}/
syn match smclFormat /{p2colset\s\+\d\+\s\+\d\+\s\+\d\+\s\+\d\+}/
syn match smclFormat /{p2col\s\+\(\|\s\+\d\+\s\+\d\+\s\+\d\+\s\+\d\+\):[^{}]*}.*{p_end}/
syn match smclFormat /{p2col\s\+\(\|\s\+\d\+\s\+\d\+\s\+\d\+\s\+\d\+\):{[^{}]*}}.*{p_end}/
syn match smclFormat /{p2line\s*\(\|\d\+\s\+\d\+\)}/
syn match smclFormat /{p2colreset}/
syn match smclFormat /{synoptset\(\|\s\+\d\+\)\(\|\s\+tabbed\|\s\+notes\)}/
syn match smclFormat /{synopthdr\(\|:[^}]\+\)}/
syn match smclFormat /{syntab\s*:[^{}]*}/
syn match smclFormat /{synopt\s*:[^{}]*}.*{p_end}/
syn match smclFormat /{synopt\s*:{[^{}]*}}.*{p_end}/
syn match smclFormat /{p2coldent\s*:[^{}]*}.*{p_end}/
syn match smclFormat /{p2coldent\s*:{[^{}]*}}.*{p_end}/
syn match smclFormat /{synoptline}/
syn match smclFormat /{bind:[^}]\+}/
syn match smclFormat /{break}/

" Comment
syn region smclComment start=/{\*/ end=/}/ oneline

" Strings
syn region smclString  matchgroup=Nothing start=/"/ end=/"/   oneline
syn region smclEString matchgroup=Nothing start=/`"/ end=/"'/ oneline contains=smclEString

" assign highlight groups

hi def link smclEString		smclString

hi def link smclCCLword		Statement
hi def link smclCCL		Type
hi def link smclFormat		Statement
hi def link smclLink		Underlined
hi def link smclComment		Comment
hi def link smclString		String

let b:current_syntax = "smcl"

" vim: ts=8
" end: syntax/smcl.vim
