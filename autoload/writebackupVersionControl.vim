" writebackupVersionControl.vim: Version control functions (diff, restore,
" history navigation) for backups made with the writebackup plugin, which have a
" date file extension in the format '.YYYYMMDD[a-z]'. 
"
" DEPENDENCIES:
"   - escapings.vim autoload script. 
"   - ingobuffer.vim autoload script. 
"   - External copy command "cp" (Unix), "copy" and "xcopy" (Windows). 
"
" Copyright: (C) 2007-2009 by Ingo Karkat
"   The VIM LICENSE applies to this script; see ':help copyright'. 
"
" Maintainer:	Ingo Karkat <ingo@karkat.de>
"
" REVISION	DATE		REMARKS 
"   2.21.013	16-Jul-2009	Added
"				g:WriteBackup_ScratchBufferCommandModifiers
"				configuration. 
"				ENH: Now issuing a warning that there are no
"				differences and closing the useless diff scratch
"				buffer if it is empty. 
"   2.21.012	14-Jul-2009	BF: Forgot {special} in shellescape() call for
"				writebackupVersionControl#ViewDiffWithPred();
"				the scratch buffer uses the ! command. 
"   2.21.011	10-Jul-2009	The creation / update of the scratch buffer
"				positions the cursor on the first line. In case
"				of a simple refresh within the diff scratch
"				buffer, the former cursor position should be
"				kept. 
"				Now checking for empty
"				g:WriteBackup_DiffShellCommand and printing
"				error. 
"				Using escapings#shellescape() for system() calls
"				instead of simply enclosing in double quotes. 
"   2.20.010	09-Jul-2009	The diff files are now saved in
"				b:WriteBackup_DiffSettings so that the diff can
"				be updated from within the diff scratch buffer. 
"   2.20.009	08-Jul-2009	ENH: Added
"				writebackupVersionControl#ViewDiffWithPred() for
"				the implemation of the
"				:WriteBackupViewDiffWithPred command
"				to show the diff output in a scratch buffer. 
"   2.11.008	24-Jun-2009	ENH: :WriteBackupDiffWithPred now takes an
"				optional [count] to diff with an earlier
"				predecessor. 
"   2.10.007	27-May-2009	Replaced simple filespec escaping with
"				built-in fnameescape() function (or emulation
"				for Vim 7.0 / 7.1) via escapings.vim wrapper. 
"				Now reducing the filespec to shortest possible
"				(:~:.) before opening a file. This avoids ugly
"				long buffer names when :set noautochdir.  
"				For the diffsplit, do not expand the
"				predecessor's filespec to an absolute
"				path any more; this is not necessary. 
"   2.01.006	11-May-2009	Backup versions are now opened read-only
"				(through :WriteBackupGoPrev/Next and
"				:WriteBackupDiffWithPred commands) to prevent
"				the user from accidentally editing the backup
"				instead of the original. 
"   2.00.005	22-Feb-2009	Added a:isForced argument to all functions that
"				implement commands which now support forcing via !. 
"				writebackupVersionControl#WriteBackupOfSavedOriginal()
"				now also observes
"				g:WriteBackup_AvoidIdenticalBackups and avoid
"				backup of the saved version if it is identical
"				to the last backup version. 
"   2.00.004	21-Feb-2009	Factored error reporting out of
"				s:GetRelativeBackup() to allow the silent usage
"				by the writebackup plugin. 
"				Replaced s:DualDigit() with printf('%02d')
"				formatting. 
"				Factored out s:VimExceptionMsg() and added catch
"				block for Vim errors in missing places. 
"				Factored s:AreIdentical() out of
"				writebackupVersionControl#IsBackedUp() and added
"				public function
"				writebackupVersionControl#IsIdenticalWithPredecessor()
"				for the writebackup plugin so that it can avoid
"				backup if an identical backup already exists. 
"				Implemented
"				writebackupVersionControl#DeleteBackup() to
"				enable the writebackup plugin to remove an
"				identical backup after the fact. 
"				Implemented
"				writebackupVersionControl#DeleteBackupLastBackup()
"				for the :WriteBackupDeleteLastBackup command, as
"				the actual delete functionality is now already
"				there. 
"				Refactored variables names and documentation. 
"   2.00.003	19-Feb-2009	ENH: Now using compare shell command configured
"				in g:WriteBackup_CompareShellCommand. 
"   2.00.002	18-Feb-2009	BF: :WriteBackupListVersions now handles (and
"				reports) backup files with a future date. (This
"				can happen when writing on a Samba share that
"				has a different clock setting.)
"				Exposed
"				writebackupVersionControl#IsOriginalFile()
"				function for writebackup plugin so that it can
"				disallow backup of backup file. 
"				BF: Didn't correctly catch writebackup.vim
"				exceptions. This could happen when running out
"				of backup names in :WriteBackupOfSavedOriginal. 
"				Added '--' argument to Unix 'cp' command so that
"				files starting with '-' are copied correctly. 
"   2.00.001	17-Feb-2009	Moved functions from plugin to separate autoload
"				script. 
"				writebackup.vim has replaced its global
"				WriteBackup_...() functions with autoload
"				functions writebackup#...(). This is an
"				incompatible change that also requires the
"				corresponding changes in here. 
"				file creation

let s:save_cpo = &cpo
set cpo&vim

let s:versionRegexp = '\.[12]\d\d\d\d\d\d\d[a-z]$'
let s:versionFileGlob = '.[12][0-9][0-9][0-9][0-9][0-9][0-9][0-9][a-z]'
let s:versionLength = 10 " 1 dot + 4 year + 2 month + 2 day + 1 letter

"- utility functions ----------------------------------------------------------
function! s:ErrorMsg( text )
    echohl ErrorMsg
    let v:errmsg = a:text
    echomsg v:errmsg
    echohl None
endfunction
function! s:WarningMsg( text )
    echohl WarningMsg
    let v:warningmsg = a:text
    echomsg v:warningmsg
    echohl None
endfunction
function! s:ExceptionMsg( exception )
    call s:ErrorMsg(substitute(v:exception, '^WriteBackup\%(VersionControl\)\?:\s*', '', ''))
endfunction
function! s:VimExceptionMsg( exception )
    " v:exception contains what is normally in v:errmsg, but with extra
    " exception source info prepended, which we cut away. 
    call s:ErrorMsg(substitute(v:exception, '^Vim\%((\a\+)\)\=:', '', ''))
endfunction

function! s:IsFileReadonly( filespec )
    return filereadable(a:filespec) && ! filewritable(a:filespec)
endfunction

function! s:FnameShortenAndEscape( filespec )
"*******************************************************************************
"* PURPOSE:
"   Escapes the passed a:filespec for use in an ex command. In addition, the
"   filespec is reduced to its shortest possible representation to avoid ugly
"   long buffer names when :set noautochdir. 
"
"* ASSUMPTIONS / PRECONDITIONS:
"	? List of any external variable, control, or other element whose state affects this procedure.
"* EFFECTS / POSTCONDITIONS:
"	? List of the procedure's effect on each external variable, control, or other element.
"* INPUTS:
"   a:filespec	File. 
"* RETURN VALUES: 
"   Shortened filespec suitable for immediate (no more commands that may change
"   the CWD!) consumption in an ex command. 
"*******************************************************************************
    return escapings#fnameescape(fnamemodify(a:filespec, ':~:.'))
endfunction

"- conversion functions -------------------------------------------------------
function! writebackupVersionControl#IsOriginalFile( filespec )
    return a:filespec !~? s:versionRegexp
endfunction

function! s:GetOriginalFilespec( filespec, isForDisplayingOnly )
"*******************************************************************************
"* PURPOSE:
"   The passed a:filespec may be any ordinary file, an original file that has
"   backups, or a backup file. In the last case, it is tried to determine the
"   original filespec. This is only guaranteed to work when backups are
"   created in the same directory as the original file. 
"* ASSUMPTIONS / PRECONDITIONS:
"   None. 
"* EFFECTS / POSTCONDITIONS:
"   None. 
"* INPUTS:
"   a:filespec	Backup or original file.
"   a:isForDisplayingOnly   If true, returns an approximation of the original
"	file for use in a user message in case it cannot be resolved. If false,
"	return an empty string in that case. 
"* RETURN VALUES: 
"   Original filespec, or empty string, or approximation. 
"*******************************************************************************
    if writebackupVersionControl#IsOriginalFile( a:filespec )
	return a:filespec
    else
	" Since a:filespec is no original file, it thusly ends with the backup
	" date file extension, and we can simply cut it off. 
	let l:adjustedBackupFilespec = strpart( a:filespec, 0, len( a:filespec ) - s:versionLength )

	let l:backupDirspec = ''
	try
	    let l:backupDirspec = writebackup#GetBackupDir(l:adjustedBackupFilespec, 1)
	catch
	    " Ignore exceptions, they just signal that the backup dir could not
	    " be determined or that a backup should not be written. We're just
	    " interested in the backup dir here, but we can live with the fact
	    " that the backup dir is unknown. 
	endtry
	if  l:backupDirspec == '.' && filereadable(l:adjustedBackupFilespec)
	    " If backups are created in the same directory, we can get the original
	    " file by stripping off the date file extension. 
	    " A buffer-local backup directory configuration which only exists for
	    " the original file buffer, but not the backup file buffer may fool us
	    " here into believing that backups are created in the same directory, so
	    " we explicitly check that the original file exists there as well. 
	    return l:adjustedBackupFilespec
	else
	    " If backups are created in a different directory, the complete filespec
	    " of the original file can not be derived from the adjusted backup
	    " filespec, as writebackup#AdjustFilespecForBackupDir() (potentially) is
	    " a one-way transformation from multiple directories to one backup
	    " directory. 
	    "
	    " TODO: Look for original file candidates in all Vim buffers via bufname(). 
	    "
	    " When we fail, return an empty string to indicate that the original
	    " filespec could not be resolved. However, if the filespec is only
	    " needed for a user message, we can generate an approximation, which is
	    " better than nothing. 
	    if a:isForDisplayingOnly
		return '???/' . fnamemodify( l:adjustedBackupFilespec, ':t' )
	    else
		return ''
	    endif
	endif
    endif
endfunction

function! s:GetVersion( filespec )
    if ! writebackupVersionControl#IsOriginalFile( a:filespec )
	return strpart( a:filespec, len( a:filespec ) - s:versionLength + 1 )
    else
	return ''
    endif
endfunction

function! s:GetAdjustedBackupFilespec( filespec )
"*******************************************************************************
"* PURPOSE:
"   The adjustedBackupFilespec is an imaginary file in the backup directory. By
"   appending a backup version, a valid backup filespec is created. 
"   In case the backup is done in the same directory as the original file, the
"   adjustedBackupFilespec is equal to the original file. 
"* ASSUMPTIONS / PRECONDITIONS:
"   None. 
"* EFFECTS / POSTCONDITIONS:
"   None. 
"* INPUTS:
"   a:filespec	Backup or original file.
"* RETURN VALUES: 
"   adjustedBackupFilespec; the backup directory may not yet exist when no
"   backups have yet been made. 
"   Throws 'WriteBackup:' or any exception resulting from query for backup dir. 
"*******************************************************************************
    if writebackupVersionControl#IsOriginalFile( a:filespec )
	return writebackup#AdjustFilespecForBackupDir( a:filespec, 1 )
    else
	return strpart( a:filespec, 0, len( a:filespec ) - s:versionLength )
    endif
endfunction

"------------------------------------------------------------------------------
function! s:VerifyIsOriginalFileAndHasPredecessor( originalFilespec, notOriginalMessage )
"*******************************************************************************
"* PURPOSE:
"   Checks that a:filespec is not a backup file and that at least one backup for
"   this file exists. If not, an error message is echoed; in the first case,
"   the passed a:notOriginalMessage is used. 
"* ASSUMPTIONS / PRECONDITIONS:
"   None. 
"* EFFECTS / POSTCONDITIONS:
"   Prints error message. 
"* INPUTS:
"   a:originalFilespec	Backup or original file (but backup file results in
"			error message). 
"   a:notOriginalMessage
"* RETURN VALUES: 
"   Empty string if verification failed; filespec of predecessor otherwise. 
"*******************************************************************************
    if ! writebackupVersionControl#IsOriginalFile( a:originalFilespec )
	call s:ErrorMsg(a:notOriginalMessage)
	return ''
    endif

    let [l:predecessor, l:errorMessage] = s:GetRelativeBackup( a:originalFilespec, -1 )
    if ! empty(l:errorMessage)
	call s:ErrorMsg(l:errorMessage)
    endif
    return l:predecessor
endfunction

"------------------------------------------------------------------------------
function! s:GetAllBackupsForFile( filespec )
"*******************************************************************************
"* PURPOSE:
"   Retrieves a list of all filespecs of backup files for a:filespec. 
"   The list is sorted from oldest to newest backup. The original filespec is
"   not part of the list. 
"* ASSUMPTIONS / PRECONDITIONS:
"   None. 
"* EFFECTS / POSTCONDITIONS:
"   None. 
"* INPUTS:
"   a:filespec	Backup or original file.
"* RETURN VALUES: 
"   Sorted list of backup filespecs. 
"*******************************************************************************
    " glob() filters out file patterns defined in 'wildignore'. If someone wants
    " to ignore backup files for command-mode file name completion and puts the
    " backup file pattern into 'wildignore', this function will break. 
    " Thus, the 'wildignore' option is temporarily reset here. 
    if has('wildignore')
	let l:save_wildignore = &wildignore
	set wildignore=
    endif
    try
	" For globbing, we need the filespec of an imaginary file in the backup
	" directory, to which we can append our file version glob. (The backup
	" files may reside in a directory different from the original file;
	" that's why we cannot simply use the original filespec for globbing.)  
	let l:adjustedBackupFilespec = s:GetAdjustedBackupFilespec(a:filespec)

	" glob() will do the right thing and return an empty list if
	" l:adjustedBackupFilespec doesn't yet exist, because no backup has yet been
	" made. 
	let l:backupFiles = split( glob( l:adjustedBackupFilespec . s:versionFileGlob ), "\n" )

	" Although the glob should already be sorted alphabetically in ascending
	" order, we'd better be sure and sort the list on our own, too. 
	let l:backupFiles = sort( l:backupFiles )
"****D echo '**** backupfiles: ' . l:backupFiles
	return l:backupFiles
    finally
	if has('wildignore')
	    let &wildignore = l:save_wildignore
	endif
    endtry

endfunction

function! s:GetIndexOfVersion( backupFiles, currentVersion )
"*******************************************************************************
"* PURPOSE:
"   Determine the index of the backup version a:currentVersion in the passed
"   list of backup files. 
"* ASSUMPTIONS / PRECONDITIONS:
"   None. 
"* EFFECTS / POSTCONDITIONS:
"   None. 
"* INPUTS:
"   a:backupFiles: List of backup filespecs. 
"   a:currentVersion: Version number of the backup filespec to be found. 
"* RETURN VALUES: 
"   Index into a:backupFiles or -1 if a:currentVersion isn't contained in
"   a:backupFiles. 
"*******************************************************************************
    let l:fileCnt = 0
    while l:fileCnt < len( a:backupFiles )
	if s:GetVersion( a:backupFiles[ l:fileCnt ] ) == a:currentVersion 
	    return l:fileCnt
	endif
	let l:fileCnt += 1
    endwhile
    return -1
endfunction
function! s:GetRelativeBackup( filespec, relativeIndex )
"*******************************************************************************
"* PURPOSE:
"   Gets the filespec of a predecessor or later version of the passed
"   filespec, regardless of whether the passed filespec is the current file
"   (without a version extension), or a versioned backup. 
"   If the index is out of bounds, the first / last available backup version
"   is returned. If a:filespec is the first / last backup version / original
"   file, an error is printed and an empty string is returned. 
"* ASSUMPTIONS / PRECONDITIONS:
"   None. 
"* EFFECTS / POSTCONDITIONS:
"   None. 
"* INPUTS:
"   a:filespec	Backup or original file.
"   a:relativeIndex Negative numbers select predecessors, positive numbers
"		    later versions. 
"* RETURN VALUES: 
"   List of 
"   - Filespec of the backup version, or empty string if no such version exists.  
"   - Error message if no such version exists. 
"   Either the first or the second list element is an empty string. 
"*******************************************************************************
    let l:backupFiles = s:GetAllBackupsForFile(a:filespec)
    let l:lastBackupIndex = len(l:backupFiles) - 1
    let l:currentIndex = (writebackupVersionControl#IsOriginalFile(a:filespec) ? l:lastBackupIndex + 1 : s:GetIndexOfVersion( l:backupFiles, s:GetVersion(a:filespec) ))

    if l:currentIndex < 0
	return ['', "Couldn't locate this backup: " . a:filespec]
    elseif l:lastBackupIndex < 0
	return ['', 'No backups exist for this file.']
    elseif a:relativeIndex > 0 && l:currentIndex == l:lastBackupIndex
	return ['', "This is the latest backup: " . a:filespec]
    elseif a:relativeIndex > 0 && l:currentIndex > l:lastBackupIndex
	return ['', 'Cannot go beyond original file.']
    elseif a:relativeIndex < 0 && l:currentIndex == 0
	return ['', 'This is the earliest backup: ' . a:filespec]
    endif

    let l:newIndex = min([max([l:currentIndex + a:relativeIndex, 0]), l:lastBackupIndex])
    return [get( l:backupFiles, l:newIndex, '' ), '']
endfunction

function! s:EditFile( filespec, isBang, isReadonly )
"*******************************************************************************
"* PURPOSE:
"   Edit a:filespec in the current window (via :edit). 
"* ASSUMPTIONS / PRECONDITIONS:
"   None. 
"* EFFECTS / POSTCONDITIONS:
"   Prints Vim error message if the file cannot be edited. 
"* INPUTS:
"   a:filespec	Backup or original file.
"   a:isBang	Flag whether any changes to the current buffer should be
"		discarded. 
"   a:isReadonly    Flag whether the file should be opened read-only. 
"* RETURN VALUES: 
"   None. 
"*******************************************************************************
    try
	execute (a:isReadonly ? 'view' : 'edit') . (a:isBang ? '!' : '') s:FnameShortenAndEscape(a:filespec)
    catch /^Vim\%((\a\+)\)\=:E/
	call s:VimExceptionMsg(v:exception)
    endtry
endfunction
function! writebackupVersionControl#WriteBackupGoOriginal( filespec, isBang )
"*******************************************************************************
"* PURPOSE:
"   Edit the original file of the passed backup file a:filespec. 
"* ASSUMPTIONS / PRECONDITIONS:
"   None. 
"* EFFECTS / POSTCONDITIONS:
"   Edits the original file in the current window, or:  
"   Prints (error) message.
"* INPUTS:
"   a:filespec	Backup or original file.
"   a:isBang	Flag whether any changes to the current buffer should be
"		discarded. 
"* RETURN VALUES: 
"   None. 
"*******************************************************************************
    try
	if writebackupVersionControl#IsOriginalFile( a:filespec )
	    echomsg 'This is the original file.'
	    return
	endif

	let l:originalFilespec = s:GetOriginalFilespec( a:filespec, 0 )
	if empty( l:originalFilespec )
	    call s:ErrorMsg('Unable to determine the location of the original file.')
	else
	    call s:EditFile(l:originalFilespec, a:isBang, 0)
	endif
    catch /^WriteBackup\%(VersionControl\)\?:/
	call s:ExceptionMsg(v:exception)
    endtry
endfunction
function! writebackupVersionControl#WriteBackupGoBackup( filespec, isBang, relativeIndex )
"*******************************************************************************
"* PURPOSE:
"   Edit a backup file version relative to the current file. 
"* ASSUMPTIONS / PRECONDITIONS:
"   None. 
"* EFFECTS / POSTCONDITIONS:
"   Edits the backup file in the current window, or:  
"   Prints error message.
"* INPUTS:
"   a:filespec	Backup or original file.
"   a:isBang	Flag whether any changes to the current buffer should be
"		discarded. 
"   a:relativeIndex 
"* RETURN VALUES: 
"   None. 
"*******************************************************************************
    try
	let [l:backupFilespec, l:errorMessage] = s:GetRelativeBackup(a:filespec, a:relativeIndex)
	if empty(l:errorMessage)
	    call s:EditFile(l:backupFilespec, a:isBang, 1)
	else
	    call s:ErrorMsg(l:errorMessage)
	endif
    catch /^WriteBackup\%(VersionControl\)\?:/
	call s:ExceptionMsg(v:exception)
    endtry
endfunction

function! writebackupVersionControl#DiffWithPred( filespec, count )
"*******************************************************************************
"* PURPOSE:
"   Creates a diff with the a:count'th predecessor of the passed a:filespec. 
"* ASSUMPTIONS / PRECONDITIONS:
"   None. 
"* EFFECTS / POSTCONDITIONS:
"   Opens a diffsplit with the predecessor, or: 
"   Prints error message.
"   Prints Vim error message if the split cannot be created. 
"* INPUTS:
"   a:filespec	Backup or original file.
"   a:count	Number of predecessors to go back. 
"* RETURN VALUES: 
"   None. 
"*******************************************************************************
    try
	let [l:predecessor, l:errorMessage] = s:GetRelativeBackup( a:filespec, -1 * a:count )
	if ! empty(l:errorMessage)
	    call s:ErrorMsg(l:errorMessage)
	else
"****D echo '**** predecessor is ' . l:predecessor
	    " Close all folds before the diffsplit; this avoids that a previous
	    " (open) fold status at the cursor position is remembered and
	    " obscures the actual differences. 
	    if has('folding') | setlocal foldlevel=0 | endif

	    " The :diffsplit command doesn't allow to open the file read-only;
	    " using ':setlocal readonly' afterwards leaves an unnecessary
	    " swapfile. Thus, we use :sview to open the file and use :diffthis
	    " on both windows. 
	    diffthis

	    let l:splittype = (g:WriteBackup_DiffVertSplit ? 'vertical ' : '') . 'sview'
	    execute l:splittype s:FnameShortenAndEscape(l:predecessor)
	    diffthis
	    " Note: We're assuming here that the ':sview l:predecessor' cannot
	    " fail, since we've just determined the predecessor via a file glob. 
	    " If we were not so sure, we would need to check and undo the
	    " :diffthis on the current window in case of failure here. 

	    " Return to original window. 
	    wincmd p
	endif
    catch /^Vim\%((\a\+)\)\=:E/
	call s:VimExceptionMsg(v:exception)
    catch /^WriteBackup\%(VersionControl\)\?:/
	call s:ExceptionMsg(v:exception)
    endtry
endfunction
function! s:GetDiffOptions( diffOptions )
    let l:vimDiffOptions = split(&diffopt, ',')
    let l:diffOptions = [
    \	g:WriteBackup_DiffCreateAlwaysArguments,
    \	(empty(a:diffOptions) ? g:WriteBackup_DiffCreateDefaultArguments : a:diffOptions),
    \	(index(l:vimDiffOptions, 'icase') == -1 ? '' : '-i'),
    \	(index(l:vimDiffOptions, 'iwhite') == -1 ? '' : '-b')
    \]
    return join( filter(l:diffOptions, '! empty(v:val)'), ' ')
endfunction
function! writebackupVersionControl#ViewDiffWithPred( filespec, count, diffOptions )
"*******************************************************************************
"* PURPOSE:
"   Shows the output of the diff with the a:count'th predecessor of the passed
"   a:filespec or of the files associated with the current diff scratch buffer
"   in a scratch buffer. 
"* ASSUMPTIONS / PRECONDITIONS:
"   None. 
"* EFFECTS / POSTCONDITIONS:
"   Opens / updates the scratch buffer with the diff output, or: 
"   Prints error message.
"   Prints Vim error message if the split cannot be created. 
"* INPUTS:
"   a:filespec	Backup or original file.
"   a:count	Number of predecessors to go back, 0 if no count was given. 
"   a:diffOptions   Optional command-line arguments passed to the diff command. 
"* RETURN VALUES: 
"   None. 
"*******************************************************************************
    try
	if empty(g:WriteBackup_DiffShellCommand)
	    throw 'WriteBackupVersionControl: No diff shell command configured. Unable to show differences to predecessor.'
	endif

	let l:save_cursor = []
	if exists('b:WriteBackup_DiffSettings')
	    " We're in a diff scratch buffer; reuse the files that were used
	    " when creating this diff. 
	    let l:scratchFilename = b:WriteBackup_DiffSettings.scratchFilename
	    let l:rootDirspec = b:WriteBackup_DiffSettings.rootDirspec
	    let l:newFile = b:WriteBackup_DiffSettings.newFile
	    if a:count
		" Another predecessor version is selected via the given [count]. 
		let [l:predecessor, l:errorMessage] = s:GetRelativeBackup(l:newFile, -1 * a:count)
		if ! empty(l:errorMessage)
		    call s:ErrorMsg(l:errorMessage)
		    return
		endif
		let l:oldFile = fnamemodify(l:predecessor, ':.')
	    else
		" No [count] was given; keep the previously used predecessor. 
		let l:oldFile = b:WriteBackup_DiffSettings.oldFile

		" Keep the current cursor position on a simple refresh of the
		" differences. 
		let l:save_cursor = getpos('.')
	    endif
	else
	    let [l:predecessor, l:errorMessage] = s:GetRelativeBackup(a:filespec, -1 * (a:count ? a:count : 1))
	    if ! empty(l:errorMessage)
		call s:ErrorMsg(l:errorMessage)
		return
	    endif

	    " Base the diff working directory on the current window's CWD. 
	    " This way, one can set the diff root through: 
	    " :lcd {diff-root} | WriteBackupViewDiffWithPred | lcd -
	    let l:rootDirspec = getcwd()

	    " The scratch file is created in the diff root, using the original file name
	    " with an appended '.diff' extension. To that, also append
	    " ' [Scratch]', to signal that this buffer doesn't persist.  
	    " Note: Because we only need the filename, not the filespec, we use
	    " s:GetAdjustedBackupFilespec(), not s:GetOriginalFilespec(); the
	    " latter one may fail when backups aren't created in the same
	    " directory as the original file. 
	    let l:scratchFilename = fnamemodify(s:GetAdjustedBackupFilespec(a:filespec), ':t') . '.diff [Scratch]'

	    let l:oldFile = fnamemodify(l:predecessor, ':.')
	    let l:newFile = fnamemodify(a:filespec, ':.')
	endif

	" Note: For the :! command, the '!' character must be escaped (cp.
	" shellescape() with {special}); we assume that in the diff options,
	" the normal escaping for ex commands has been done by the user. 
	" Note: Specify filespecs relative to the diff root, i.e. the
	" current window's CWD. 
	let l:diffCmd = printf('%s %s %s %s', g:WriteBackup_DiffShellCommand,
	\	escape(s:GetDiffOptions(a:diffOptions), '!'),
	\	escapings#shellescape(l:oldFile, 1),
	\	escapings#shellescape(l:newFile, 1)
	\)

	if ! ingobuffer#MakeScratchBuffer(
	\	l:rootDirspec,
	\	l:scratchFilename,
	\	1,
	\	'silent 1read !' . l:diffCmd,
	\	g:WriteBackup_ScratchBufferCommandModifiers . ' new'
	\)
	    return
	endif
	
	if line('$') == 1 && empty(getline(1))
	    " The diff scratch buffer is empty: There are no differences, so
	    " discard the useless window and show a warning instead. 
	    bdelete

	    redraw
	    let l:savedMsg = (&l:modified ? 'saved ' : '') 
	    call s:WarningMsg(printf("No differences reported between %s and backup '%s' of '%s'.",
	    \	(writebackupVersionControl#IsOriginalFile(a:filespec) ?
	    \	    'the current ' . l:savedMsg . 'version' :
	    \	    l:savedMsg . "backup '" . s:GetVersion(a:filespec) . "'"
	    \	),
	    \	s:GetVersion(l:predecessor),
	    \	s:GetOriginalFilespec(a:filespec, 1)
	    \))

	    return
	endif

	setlocal filetype=diff

	" Save the files that participate in the diff so that the diff can be
	" updated from within the diff scratch buffer by re-executing
	" :WriteBackupViewDiffWithPred. 
	" Also save the actual scratch buffer name to correctly handle renamings
	" of the diff scratch buffer via :saveas or :file. 
	let b:WriteBackup_DiffSettings = {
	\	'rootDirspec' : l:rootDirspec,
	\	'oldFile' : l:oldFile,
	\	'newFile' : l:newFile,
	\	'scratchFilename' : fnamemodify(bufname(''), ':t')
	\}

	" The creation / update of the scratch buffer positions the cursor on
	" the first line. In case of a simple refresh within the diff scratch
	" buffer, the former cursor position should be kept. 
	if ! empty(l:save_cursor)
	    call setpos('.', l:save_cursor)
	endif

	redraw
	echo l:diffCmd
    catch /^Vim\%((\a\+)\)\=:E/
	call s:VimExceptionMsg(v:exception)
    catch /^WriteBackup\%(VersionControl\)\?:/
	call s:ExceptionMsg(v:exception)
    endtry
endfunction

function! s:EchoElapsedTimeSinceVersion( backupFilespec )
"*******************************************************************************
"* PURPOSE:
"   Informs the user about the elapsed time since the passed a:backupFilespec has
"   been modified. 
"* ASSUMPTIONS / PRECONDITIONS:
"   None. 
"* EFFECTS / POSTCONDITIONS:
"   Prints message. 
"* INPUTS:
"   a:backupFilespec	Backup file. 
"
"* RETURN VALUES: 
"   None. 
"*******************************************************************************
    let l:timeElapsed = localtime() - getftime( a:backupFilespec )
    let l:isBackupInFuture = 0
    if l:timeElapsed < 0
	let l:timeElapsed = -1 * l:timeElapsed
	let l:isBackupInFuture = 1
    endif

    let l:secondsElapsed = l:timeElapsed % 60
    let l:minutesElapsed = (l:timeElapsed / 60) % 60
    let l:hoursElapsed = (l:timeElapsed / 3600) % 24
    let l:daysElapsed = (l:timeElapsed / (3600 * 24))

    let l:message = printf(
    \	(l:isBackupInFuture ?
    \	    'The last backup has a modification date %s%02d:%02d:%02d in the future?!' :
    \	    'The last backup was done %s%02d:%02d:%02d ago.'
    \	),
    \	(l:daysElapsed > 0 ? l:daysElapsed . ' days, ' : ''),
    \	l:hoursElapsed,
    \	l:minutesElapsed,
    \	l:secondsElapsed
    \)

    echomsg l:message
endfunction
function! s:GetBackupDir( originalFilespec )
"*******************************************************************************
"* PURPOSE:
"   Resolves the directory that contains the backup files. 
"* ASSUMPTIONS / PRECONDITIONS:
"   None. 
"* EFFECTS / POSTCONDITIONS:
"   None. 
"* INPUTS:
"   a:originalFilespec
"* RETURN VALUES: 
"   Dirspec representing the backup directory, '.' if equal to the original
"   file's directory. 
"   Throws 'WriteBackup:' or any exception resulting from query for backup dir. 
"*******************************************************************************
    let l:backupDirspec = writebackup#GetBackupDir(a:originalFilespec, 1)
    if l:backupDirspec ==# '.'
	return l:backupDirspec
    endif

    " Convert both original file's directory and backup directory to absolute
    " paths in order to compare for equality. 
    let l:originalDirspec = fnamemodify(a:originalFilespec, ':p:h')
    " Note: Must use :p:h modifier on dirspec to remove trailing path separator
    " left by :p. 
    let l:absoluteBackupDirspec = fnamemodify(l:backupDirspec, ':p:h')
    if l:absoluteBackupDirspec ==# l:originalDirspec
	return '.'
    endif

    " The backup dir is (or at least, looks) different from the original file's. 
    " Return either full absolute path or relative to home directory, if
    " possible. 
    return fnamemodify(l:absoluteBackupDirspec, ':~')
endfunction
function! writebackupVersionControl#ListVersions( filespec )
"*******************************************************************************
"* PURPOSE:
"   Shows the user a list of all available versions for a:filespec. 
"* ASSUMPTIONS / PRECONDITIONS:
"   None. 
"* EFFECTS / POSTCONDITIONS:
"   Prints list of versions, or: 
"   Prints error message.
"* INPUTS:
"   a:filespec	Backup or original file.
"* RETURN VALUES: 
"   None. 
"*******************************************************************************
    try
	let l:originalFilespec = s:GetOriginalFilespec( a:filespec, 1 )
	let l:currentVersion = s:GetVersion( a:filespec )
	let l:backupDirspec = s:GetBackupDir(l:originalFilespec)
	let l:backupFiles = s:GetAllBackupsForFile(a:filespec)
	if empty( l:backupFiles )
	    echomsg "No backups exist for this file."
	    return
	endif

	let l:versionMessageHeader = "These backups exist for file '" . l:originalFilespec . "'" . (l:backupDirspec =~# '^\.\?$' ? '' : ' in ' . l:backupDirspec)
	let l:versionMessageHeader .= ( empty(l:currentVersion) ? ': ' : ' (current version is marked >x<): ')
	echomsg l:versionMessageHeader

	let l:versionMessage = ''
	let l:backupVersion = ''
	for l:backupFile in l:backupFiles
	    let l:previousVersion = l:backupVersion
	    let l:backupVersion = s:GetVersion( l:backupFile )
	    if strpart( l:backupVersion, 0, len(l:backupVersion) - 1 ) == strpart( l:previousVersion, 0, len(l:previousVersion) - 1 )
		let l:versionMessageAddition = strpart( l:backupVersion, len(l:backupVersion) - 1 )
		if l:backupVersion == l:currentVersion
		    let l:versionMessageAddition = '>' . l:versionMessageAddition . '<'
		endif
		let l:versionMessage .= l:versionMessageAddition
	    else
		echomsg l:versionMessage 
		let l:versionMessage = l:backupVersion
		if l:backupVersion == l:currentVersion
		    let l:versionMessage= strpart( l:versionMessage, 0, len(l:versionMessage) - 1 ). '>' . strpart( l:versionMessage, len(l:versionMessage) - 1 ) . '<'
		endif
	    endif
	endfor
	echomsg l:versionMessage

	if empty( l:currentVersion )
	    let l:lastBackupFile = l:backupFiles[-1]
	    call s:EchoElapsedTimeSinceVersion( l:lastBackupFile )
	endif
    catch /^WriteBackup\%(VersionControl\)\?:/
	call s:ExceptionMsg(v:exception)
    endtry
endfunction

function! s:AreIdentical( filespec1, filespec2 )
"*******************************************************************************
"* PURPOSE:
"   Test whether two files have identical contents. 
"* ASSUMPTIONS / PRECONDITIONS:
"   None. 
"* EFFECTS / POSTCONDITIONS:
"   May invoke external shell command. 
"* INPUTS:
"   a:filespec1, a:filespec2	    Filespecs of the files. 
"* RETURN VALUES: 
"   Boolean whether the file contents are identical. 
"   Throws 'WriteBackupVersionControl: Encountered problems...' 
"*******************************************************************************
    " Optimization: First compare the file sizes, as this is much faster than
    " performing an actual comparison; we're not interested in the differences,
    " anyway, only if there *are* any!
    if getfsize( a:filespec1 ) != getfsize( a:filespec2 )
	return 0
    endif

    if empty(g:WriteBackup_CompareShellCommand)
	throw 'WriteBackupVersionControl: No compare shell command configured. Unable to compare with latest backup.'
    endif

    " Expand filespecs to absolute paths to avoid problems with CWD, especially
    " on Windows systems with UNC paths. 
    let l:diffCmd = printf('%s %s %s', 
    \	g:WriteBackup_CompareShellCommand,
    \	escapings#shellescape(fnamemodify(a:filespec1, ':p')),
    \	escapings#shellescape(fnamemodify(a:filespec2, ':p'))
    \)

    " Using the system() command even though we're not interested in the command
    " output (which is suppressed via command-line arguments to the compare
    " shell command, anyway). This is because on Windows GVIM, the system() call
    " does not (briefly) open a Windows shell window, but ':silent !{cmd}' does. 
    " system() also does not unintentionally trigger the 'autowrite' feature. 
    call system( l:diffCmd )
"****D echo '**** ' . g:WriteBackup_CompareShellCommand . ' return code=' . v:shell_error

    if v:shell_error == 0
	return 1
    elseif v:shell_error == 1
	return 0
    else
	throw printf("WriteBackupVersionControl: Encountered problems with '%s' invocation. Unable to compare with latest backup.", g:WriteBackup_CompareShellCommand)
    endif
endfunction
function! writebackupVersionControl#IsIdenticalWithPredecessor( filespec )
"*******************************************************************************
"* PURPOSE:
"   Queries whether the predecessor of the passed (backup or original) file is
"   has the same contents. 
"* ASSUMPTIONS / PRECONDITIONS:
"   None. 
"* EFFECTS / POSTCONDITIONS:
"   None. 
"* INPUTS:
"   a:filespec	Backup or original file. 
"* RETURN VALUES: 
"   Backup version of the identical predecessor or empty string to indicate that
"   either no backup exists, or the predecessor is different. 
"   Throws 'WriteBackupVersionControl: Encountered problems...' 
"*******************************************************************************
    let [l:predecessor, l:errorMessage] = s:GetRelativeBackup( a:filespec, -1 )
    if ! empty(l:errorMessage)
	" No predecessor exists. 
	return ''
    endif

    if s:AreIdentical(l:predecessor, a:filespec)
	return s:GetVersion(l:predecessor)
    else
	return ''
    endif
endfunction
function! writebackupVersionControl#IsBackedUp( originalFilespec )
"*******************************************************************************
"* PURPOSE:
"   Informs the user whether there exists a backup for the passed
"   a:originalFilespec file. 
"* ASSUMPTIONS / PRECONDITIONS:
"   None. 
"* EFFECTS / POSTCONDITIONS:
"   Prints backup status, or: 
"   Prints error message. 
"* INPUTS:
"   a:originalFilespec	Backup or original file (but backup file results in
"			error message). 
"* RETURN VALUES: 
"   None. 
"*******************************************************************************
    try
	let l:predecessor = s:VerifyIsOriginalFileAndHasPredecessor( a:originalFilespec, 'You can only check the backup status of the original file, not of backups!' )
	if empty( l:predecessor )
	    return
	endif

	" As we compare the predecessor with the saved original file, not the actual
	" buffer contents (and this is what the user typically wants; checking
	" whether it is safe to write this buffer because an update exists), we add
	" a hint to the user message if the buffer is indeed modified. 
	let l:savedMsg = (&l:modified ? 'saved ' : '') 

	if s:AreIdentical(l:predecessor, a:originalFilespec)
	    echomsg printf("The current %sversion of '%s' is identical with the latest backup '%s'.", l:savedMsg, a:originalFilespec, s:GetVersion(l:predecessor))
	else
	    call s:WarningMsg(printf("The current %sversion of '%s' is different from the latest backup '%s'.", l:savedMsg, a:originalFilespec, s:GetVersion(l:predecessor)))
	endif
    catch /^WriteBackup\%(VersionControl\)\?:/
	call s:ExceptionMsg(v:exception)
    endtry
endfunction

function! s:Copy( source, target, isForced )
"*******************************************************************************
"* PURPOSE:
"   Copies a:source to a:target. If a:target exists, it is overwritten (unless
"   is is readonly, then the copy command will fail, unless a:isForced). 
"* ASSUMPTIONS / PRECONDITIONS:
"   None. 
"* EFFECTS / POSTCONDITIONS:
"   Creates / modifies a:target on the file system. 
"* INPUTS:
"   a:source filespec
"   a:target filespec
"   a:isForced	Flag whether readonly targets should be overwritten. 
"* RETURN VALUES: 
"   None. 
"   Throws 'WriteBackupVersionControl: Unsupported operating system type.'
"   Throws copy command output if shell copy command failed. 
"*******************************************************************************
    " Expand filespecs to absolute paths to avoid problems with CWD, especially
    " on Windows systems with UNC paths. 
    let l:sourceFilespec = fnamemodify( a:source, ':p' )
    let l:targetFilespec = fnamemodify( a:target, ':p' )

    if has('win32') || has('win64')
	" On Windows, 'copy' cannot overwrite a readonly target; only 'xcopy'
	" can (with the /R option). 
	let l:copyShellCmd = (s:IsFileReadonly(a:target) ? 'xcopy /Q /R /Y' : 'copy /Y')
    elseif has('unix')
	let l:copyShellCmd = (s:IsFileReadonly(a:target) ? 'cp -f' : 'cp') . ' --'
    else
	throw 'WriteBackupVersionControl: Unsupported operating system type.'
    endif
    let l:copyCmd = printf('%s %s %s',
    \	l:copyShellCmd,
    \	escapings#shellescape(l:sourceFilespec),
    \	escapings#shellescape(l:targetFilespec)
    \)

    let l:cmdOutput = system(l:copyCmd)
    if v:shell_error != 0
	throw l:cmdOutput
    endif
endfunction
function! s:Restore( source, target, isForced, confirmationMessage )
"*******************************************************************************
"* PURPOSE:
"   Restores a:source over an existing a:target. The user is asked to confirm
"   this destructive operation, using the passed a:confirmationMessage (unless
"   a:isForced). 
"* ASSUMPTIONS / PRECONDITIONS:
"   None. 
"* EFFECTS / POSTCONDITIONS:
"   Modifies a:target on the file system. 
"* INPUTS:
"   a:source filespec
"   a:target filespec
"   a:isForced	Flag whether restore should proceed without confirmation and
"		overwrite readonly original file. 
"   a:confirmationMessage
"* RETURN VALUES: 
"   Boolean indicating whether the file has actually been restored. 
"   Throws 'WriteBackupVersionControl: Cannot overwrite readonly
"	    '<original-file>' (add ! to override)' (unless a:isForced). 
"   Throws 'WriteBackupVersionControl: Failed to restore file: <reason>'
"*******************************************************************************
    if ! a:isForced
	if s:IsFileReadonly(a:target)
	    throw printf("WriteBackupVersionControl: Cannot overwrite readonly '%s' (add ! to override)", a:target)
	else
	    let l:response = confirm( a:confirmationMessage, "&No\n&Yes", 1, 'Question' )
	    if l:response != 2
		echomsg 'Restore canceled.'
		return 0
	    endif
	endif
    endif

    " We could restore using only Vim functionality:
    "	edit! a:target
    " 	normal ggdG
    " 	0read a:source
    " 	write
    " That would have the following disadvantages: 
    " - the target's modification date would be different from the one
    "   of the source, which would fool superficial synchronization tools. 
    " - There's the (small) risk that Vim autocmds or settings like
    "   'fileencoding' or 'fileformat' are now different from when the backup
    "   was written, and may thus lead to conversion errors or different file
    "   contents. 
    " Thus, we invoke an external command to create a perfect copy.
    " Unfortunately, this introduces platform-specific code. 
    try
	call s:Copy(a:source, a:target, a:isForced)
    catch
	throw 'WriteBackupVersionControl: Failed to restore file: ' . v:exception
    endtry
    return 1
endfunction
function! writebackupVersionControl#RestoreFromPred( originalFilespec, isForced )
"*******************************************************************************
"* PURPOSE:
"   Restores the passed original file with its latest backup. 
"* ASSUMPTIONS / PRECONDITIONS:
"   None. 
"* EFFECTS / POSTCONDITIONS:
"   Reloads the restored file, or: 
"   Prints error message. 
"* INPUTS:
"   a:originalFilespec	Backup or original file (but backup file results in
"			error message). 
"   a:isForced	Flag whether restore should proceed without confirmation and
"		overwrite readonly original file. 
"* RETURN VALUES: 
"   None. 
"*******************************************************************************
    try
	let l:predecessor = s:VerifyIsOriginalFileAndHasPredecessor( a:originalFilespec, 'You can only restore the original file, not a backup!' )
	if empty(l:predecessor)
	    return
	endif

	if s:Restore( l:predecessor, a:originalFilespec, a:isForced, printf("Really override this file with backup '%s'?", s:GetVersion(l:predecessor) ))
	    edit!
	endif
    catch /^Vim\%((\a\+)\)\=:E/
	call s:VimExceptionMsg(v:exception)
    catch /^WriteBackup\%(VersionControl\)\?:/
	call s:ExceptionMsg(v:exception)
    endtry
endfunction
function! writebackupVersionControl#RestoreThisBackup( filespec, isForced )
"*******************************************************************************
"* PURPOSE:
"   Restores the passed file as the original file. 
"* ASSUMPTIONS / PRECONDITIONS:
"   None. 
"* EFFECTS / POSTCONDITIONS:
"   Loads the restored original file, or: 
"   Prints error message. 
"* INPUTS:
"   a:filespec	Backup file. 
"   a:isForced	Flag whether restore should proceed without confirmation and
"		overwrite readonly original file. 
"* RETURN VALUES: 
"   None. 
"*******************************************************************************
    try
	let l:currentVersion = s:GetVersion(a:filespec)
	if empty(l:currentVersion)
	    call s:ErrorMsg('You can only restore backup files!')
	    return
	endif

	let l:originalFilespec = s:GetOriginalFilespec(a:filespec, 0)
	if empty( l:originalFilespec )
	    " TODO: 'Unable to determine the location of the original file; open it in another buffer.'
	    call s:ErrorMsg('Unable to determine the location of the original file.')
	    return
	endif

	if s:Restore( a:filespec, l:originalFilespec, a:isForced, printf("Really override '%s' with this backup '%s'?", l:originalFilespec, l:currentVersion) )
	    execute 'edit! ' . s:FnameShortenAndEscape(l:originalFilespec)
	endif
    catch /^Vim\%((\a\+)\)\=:E/
	call s:VimExceptionMsg(v:exception)
    catch /^WriteBackup\%(VersionControl\)\?:/
	call s:ExceptionMsg(v:exception)
    endtry
endfunction

function! writebackupVersionControl#WriteBackupOfSavedOriginal( originalFilespec, isForced )
"*******************************************************************************
"* PURPOSE:
"   Instead of backing up the current buffer, back up the saved version of the
"   buffer. 
"* ASSUMPTIONS / PRECONDITIONS:
"   None. 
"* EFFECTS / POSTCONDITIONS:
"   Writes backup, or:  
"   Prints error message. 
"* INPUTS:
"   a:originalFilespec	Original file.
"   a:isForced	Flag whether creation of a new backup file is forced, i.e. even
"		if contents are identical or when no more backup versions (for
"		this day) are available. 
"* RETURN VALUES: 
"   Throws 'WriteBackupVersionControl: You can only backup the latest file version, not a backup file itself!'
"   Throws 'WriteBackupVersionControl: This file is already backed up as '<version>''. 
"   Throws 'WriteBackup:' or any exception resulting from query for backup dir. 
"   Throws 'WriteBackupVersionControl: Encountered problems...' 
"*******************************************************************************
    try
	if ! writebackupVersionControl#IsOriginalFile(a:originalFilespec)
	    throw 'WriteBackupVersionControl: You can only backup the latest file version, not a backup file itself!'
	endif

	if g:WriteBackup_AvoidIdenticalBackups && ! a:isForced
	    let l:currentBackupVersion = writebackupVersionControl#IsIdenticalWithPredecessor(a:originalFilespec)
	    if ! empty(l:currentBackupVersion)
		throw printf("WriteBackupVersionControl: This file is already backed up as '%s'", l:currentBackupVersion)
	    endif
	endif

	let l:backupFilename = writebackup#GetBackupFilename(a:originalFilespec, a:isForced)
	call s:Copy(a:originalFilespec, l:backupFilename, a:isForced)
	echomsg '"' . l:backupFilename . '" written'
    catch /^WriteBackup\%(VersionControl\)\?:/
	call s:ExceptionMsg(v:exception)
    catch
	call s:ErrorMsg('Failed to backup file: ' . v:exception)
    endtry
endfunction

function! writebackupVersionControl#DeleteBackup( backupFilespec, isForced )
"*******************************************************************************
"* PURPOSE:
"   Delete the passed backup file, if it is writable (or forced). 
"* ASSUMPTIONS / PRECONDITIONS:
"   The file a:backupFilespec exists. 
"* EFFECTS / POSTCONDITIONS:
"   Removes the passed backup file from the file system. 
"* INPUTS:
"   a:backupFilespec	Backup file. 
"   a:isForced	Flag whether readonly backups should also be deleted. 
"* RETURN VALUES: 
"   None. 
"   Throws 'WriteBackupVersionControl: Cannot delete original file!'
"   Throws 'WriteBackupVersionControl: Cannot delete readonly backup '<version>'
"	    (add ! to override)' (unless a:isForced). 
"   Throws 'WriteBackupVersionControl: Failed to delete backup version
"	    '<version>''
"*******************************************************************************
    if writebackupVersionControl#IsOriginalFile(a:backupFilespec)
	throw 'WriteBackupVersionControl: Cannot delete original file!'
    endif

    " The delete() function also deletes readonly files without complaining, so
    " we need to explicitly check for readonly files to avoid that. 
    if ! a:isForced && s:IsFileReadonly(a:backupFilespec)
	throw printf("WriteBackupVersionControl: Cannot delete readonly backup '%s' (add ! to override)", s:GetVersion(a:backupFilespec))
    endif

    if delete(a:backupFilespec) != 0
	throw printf("WriteBackupVersionControl: Failed to delete backup '%s'", s:GetVersion(a:backupFilespec))
    endif
endfunction
function! writebackupVersionControl#DeleteBackupLastBackup( filespec, isForced )
"*******************************************************************************
"* PURPOSE:
"   Delete the latest backup version. 
"* ASSUMPTIONS / PRECONDITIONS:
"   None. 
"* EFFECTS / POSTCONDITIONS:
"   Removes the last backup file from the file system. 
"   Prints (error) message. 
"* INPUTS:
"   a:filespec	Backup or original file.
"   a:isForced	Flag whether delete without confirmation and whether readonly
"		backups should also be deleted. 
"* RETURN VALUES: 
"   None. 
"*******************************************************************************
    try
	let l:backupFiles = s:GetAllBackupsForFile(a:filespec)
	if len(l:backupFiles) == 0
	    call s:ErrorMsg('No backups exist for this file.')
	    return
	endif
	let l:lastBackupFile = l:backupFiles[-1]

	if ! a:isForced
	    let l:response = confirm( printf("Really delete backup '%s'?", s:GetVersion(l:lastBackupFile)), "&No\n&Yes", 1, 'Question' )
	    if l:response != 2
		echomsg 'Delete canceled.'
		return
	    endif
	endif

	call writebackupVersionControl#DeleteBackup(l:lastBackupFile, a:isForced)

	echomsg printf("Deleted backup '%s'; %s", 
	\   s:GetVersion(l:lastBackupFile),
	\   (len(l:backupFiles) == 1 ? 'no backups exist for this file any more.' : "last backup now is '" . s:GetVersion(l:backupFiles[-2]) . "'")
	\)
    catch /^WriteBackup\%(VersionControl\)\?:/
	call s:ExceptionMsg(v:exception)
    endtry
endfunction

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sts=4 sw=4 noexpandtab ff=unix fdm=syntax :
