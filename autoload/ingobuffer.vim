" ingobuffer.vim: Custom buffer functions. 
"
" Copyright: (C) 2009 by Ingo Karkat
"   The VIM LICENSE applies to this script; see ':help copyright'. 
"
" Maintainer:	Ingo Karkat <ingo@karkat.de>
"
" REVISION	DATE		REMARKS 
"	001	05-Jan-2009	file creation

function! ingobuffer#CombineToFilespec( dirspec, filename )
    " Use path separator as exemplified by the passed dirspec. 
    let l:pathSeparator = (a:dirspec =~# '\' && a:dirspec !~# '/' ? '\' : '/')
    return a:dirspec . (a:dirspec =~# '^$\|[/\\]$' ? '' : l:pathSeparator) . a:filename
endfunction

function! ingobuffer#NextScratchFilename( filespec )
    if a:filespec !~# ' \[\h\+\d*\]$'
	return a:filespec . ' [Scratch]'
    elseif a:filespec !~# ' \[\h\+\d\+\]$'
	return substitute(a:filespec, '\]$', '1]', '')
    else
	let l:number = matchstr(a:filespec, ' \[\h\+\zs\d\+\ze\]$')
	return substitute(a:filespec, '\d\+\]$', (l:number + 1) . ']', '')
    endif
endfunction
function! s:Bufnr( dirspec, filename )
    return bufnr(
    \	escapings#bufnameescape(
    \	    fnamemodify(
    \		ingobuffer#CombineToFilespec(a:dirspec, a:filename),
    \		'%:p'
    \	    )
    \   )
    \)
endfunction
function! s:ChangeDir( dirspec )
    if empty( a:dirspec )
	return
    endif
    execute 'lcd ' . escapings#fnameescape(a:dirspec)
endfunction
function! s:BufType( scratchIsFile )
    return (a:scratchIsFile ? 'nowrite' : 'nofile')
endfunction
function! ingobuffer#MakeScratchBuffer( scratchDirspec, scratchFilename, scratchIsFile, scratchCommand, windowOpenCommand )
"*******************************************************************************
"* PURPOSE:
"   Create (or re-use an existing) scratch buffer (i.e. doesn't correspond to a
"   file on disk, but can be saved as such). 
"   To keep the scratch buffer (and create a new scratch buffer on the next
"   invocation), rename the current scratch buffer via ':file <newname>', or
"   make it a normal buffer via ':setl buftype='. 
"
"* ASSUMPTIONS / PRECONDITIONS:
"   None. 
"* EFFECTS / POSTCONDITIONS:
"   Creates or opens scratch buffer and loads it in a window (as specified by
"   a:windowOpenCommand) and activates that window. 
"* INPUTS:
"   a:scratchDirspec	Local working directory for the scratch buffer
"			(important for :! scratch commands). Pass empty string
"			to maintain the current CWD as-is. Pass '.' to maintain
"			the CWD but also fix it via :lcd. 
"			(Attention: ':set autochdir' will reset any CWD once the
"			current window is left!) Pass the getcwd() output if
"			maintaining the current CWD is important for
"			a:scratchCommand. 
"   a:scratchFilename	The name for the scratch buffer, so it can be saved via
"			either :w! or :w <newname>. 
"   a:scratchIsFile	Flag whether the scratch buffer should behave like a
"			file (i.e. adapt to changes in the global CWD), or not. 
"   a:scratchCommand	Ex :read command to populate the scratch buffer. Use
"			:1read so that the first empty line will be kept; it is
"			deleted automatically. 
"   a:windowOpenCommand	Ex command to open the scratch window, e.g. :vnew or
"			:topleft new. 
"* RETURN VALUES: 
"   Indicator whether the scratch buffer has been opened: 
"   0	Failed to open scratch buffer. 
"   1	Already in scratch buffer window. 
"   2	Jumped to open scratch buffer window. 
"   3	Loaded existing scratch buffer in new window. 
"   4	Created scratch buffer in new window. 
"*******************************************************************************
    let l:currentWinNr = winnr()
    let l:status = 0

    let l:scratchBufnr = s:Bufnr(a:scratchDirspec, a:scratchFilename)
    let l:scratchWinnr = bufwinnr(l:scratchBufnr)
"****D echomsg '**** bufnr=' . l:scratchBufnr 'winnr=' . l:scratchWinnr
    if l:scratchWinnr == -1
	if l:scratchBufnr == -1
	    execute a:windowOpenCommand
	    " Note: The directory must already be changed here so that the :file
	    " command can set the correct buffer filespec. 
	    call s:ChangeDir(a:scratchDirspec)
	    execute 'silent keepalt file ' . escapings#fnameescape(a:scratchFilename)
	    let l:status = 4
	elseif getbufvar(l:scratchBufnr, '&buftype') =~# s:BufType(a:scratchIsFile)
	    execute a:windowOpenCommand
	    execute l:scratchBufnr . 'buffer'
	    let l:status = 3
	else
	    " A buffer with the scratch filespec is already loaded, but it
	    " contains an existing file, not a scratch file. As we don't want to
	    " jump to this existing file, try again with the next scratch
	    " filename. 
	    return ingobuffer#MakeScratchBuffer(a:scratchDirspec, ingobuffer#NextScratchFilename(a:scratchFilename), a:scratchIsFile, a:scratchCommand, a:windowOpenCommand)
	endif
    else
	if l:scratchWinnr == l:currentWinNr
	    let l:status = 1
	else
	    execute l:scratchWinnr . 'wincmd w'
	    let l:status = 2
	endif
    endif

    call s:ChangeDir(a:scratchDirspec)
    setlocal noreadonly
    silent normal! ggdG
    " Note: ':silent' to suppress the "--No lines in buffer--" message. 

    execute a:scratchCommand
    " ^ Keeps the existing line at the top of the buffer. 
    " v Deletes it. 
    normal 1Gdd

    execute 'setlocal buftype=' . s:BufType(a:scratchIsFile)
    setlocal bufhidden=wipe nobuflisted noswapfile readonly
    return l:status
endfunction

" vim: set sts=4 sw=4 noexpandtab ff=unix fdm=syntax :
