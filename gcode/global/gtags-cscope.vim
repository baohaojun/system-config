" File: gtags-cscope.vim
" Author: Tama Communications Corporation
" Version: 0.2
" Last Modified: Feb 5, 2010
"
" Copyright and lisence
" ---------------------
" Copyright (c) 2010 Tama Communications Corporation
"
" This file is part of GNU GLOBAL.
"
" This program is free software: you can redistribute it and/or modify
" it under the terms of the GNU General Public License as published by
" the Free Software Foundation, either version 3 of the License, or
" (at your option) any later version.
" 
" This program is distributed in the hope that it will be useful,
" but WITHOUT ANY WARRANTY; without even the implied warranty of
" MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
" GNU General Public License for more details.
" 
" You should have received a copy of the GNU General Public License
" along with this program.  If not, see <http://www.gnu.org/licenses/>.
"
" Overview
" --------
" The gtags-cscope.vim plugin script integrates the GNU GLOBAL source code tag system
" with Vim using cscope interface.
"
" Installation
" ------------
" Drop the file in your plugin directory or source it from your vimrc.
" To use this script, you need the GNU GLOBAL-5.8 or later installed
" in your machine.
"
" Usage
" -----
" First of all, you must execute gtags(1) at the root of source directory
" to make tag files. Assuming that your source directory is '/var/src',
" it is neccessary to execute the following commands.
"
"	$ cd /var/src
"	$ gtags
"	$ vim
"
" Basic command
" -------------
" Then you can use cs commands except for the 'd'(2) command.
" Profitable commands are assigned to keys like follows:
"	Find definition		<C-\>t :cs find g
"	Find reference		<C-\>r :cs find c
"	Find other symbol	<C-\>s :cs find s
"	Find egrep pattern	<C-\>g :cs find e
"	Find path		<C-\>P :cs find f
"
" You can move tag list using:
"	Go to the next tag	<C-\><C-n> :tn
"	Go to the previous tag	<C-\><C-p> :tp
"	Pop tag stack		<C-t> :pop
"
" About the other tag command, you can see the help like this:
"
"          :h tagsrch
"
" Enhancing command
" -----------------
" You can use the context jump function. To use this function, put the cursor
" on a word and type <C-\><C-\><C-]>.
" If you can use mouse then please double click on the left button.
" To pop tag, please type 'g' and click on the right button.
"
" Configure
" ---------
" To avoid pushy key and mouse mappings:
"	let GtagsCscope_Auto_Map = 0	(in .vimrc)
" To avoid auto loading:
"	let GtagsCscope_Auto_Load = 0	(in .vimrc)
" To load gtagscscope by hand:
"	:GtagsCscope <ENTER>		(in vim command line)
" To use 'vim -t ', ':tag' and '<C-]>'
"	set cscopetag			(in .vimrc or vim command line)
"
if exists("loaded_gtags_cscope")
    finish
endif
if !has("cscope")
    finish
endif
if !exists("GtagsCscope_Auto_Load")
    let GtagsCscope_Auto_Load = 1
endif
if !exists("GtagsCscope_Auto_Map")
    let GtagsCscope_Auto_Map = 1
endif

"
" Display error message.
"
function! s:Error(msg)
    echohl WarningMsg |
           \ echomsg 'Gtags-cscope: ' . a:msg |
           \ echohl None
endfunction

function! s:GtagsCscope_GtagsRoot()
    let cmd = "global -pq"
    let cmd_output = system(cmd)
    if v:shell_error != 0
        if v:shell_error == 3
            call s:Error('GTAGS not found.')
        else
            call s:Error('global command failed. command line: ' . cmd)
        endif
        return ''
    endif
    return strpart(cmd_output, 0, strlen(cmd_output) - 1)
endfunction

function! s:GtagsCscope()
    "
    " Get gtagsroot directory.
    "
    let gtagsroot = s:GtagsCscope_GtagsRoot()
    if gtagsroot == ''
        return
    endif
    "
    " Load gtags-cscope.
    "
    set csprg=gtags-cscope
    exe "cs add " . gtagsroot . "/GTAGS"
    set csto=0
    "
    " Key mapping
    "
    if g:GtagsCscope_Auto_Map == 1
        " Context search. See the --from-here option of global(1).
        :map <C-\><C-\><C-]> :cs find d <C-R>=expand("<cword>")<CR>:<C-R>=line('.')<CR>:%<CR>
        " normal command
        :nmap <C-\>t :cs find g <C-R>=expand("<cword>")<CR>
        :nmap <C-\>r :cs find c <C-R>=expand("<cword>")<CR>
        :nmap <C-\>s :cs find s <C-R>=expand("<cword>")<CR>
        :nmap <C-\>g :cs find e <C-R>=expand("<cword>")<CR>
        :nmap <C-\>P :cs find f 
        " Using 'CTRL-spacebar', the result is displayed in new horizontal window.
        :nmap <C-@>t :scs find g <C-R>=expand("<cword>")<CR>
        :nmap <C-@>r :scs find c <C-R>=expand("<cword>")<CR>
        :nmap <C-@>s :scs find s <C-R>=expand("<cword>")<CR>
        :nmap <C-@>g :scs find e <C-R>=expand("<cword>")<CR>
        :nmap <C-@>P :scs find f 
        " Hitting CTRL-space *twice*, the result is displayed in new vertical window.
        :nmap <C-@><C-@>t :vert scs find g <C-R>=expand("<cword>")<CR>
        :nmap <C-@><C-@>r :vert scs find c <C-R>=expand("<cword>")<CR>
        :nmap <C-@><C-@>s :vert scs find s <C-R>=expand("<cword>")<CR>
        :nmap <C-@><C-@>g :vert scs find e <C-R>=expand("<cword>")<CR>
        :nmap <C-@><C-@>P :vert scs find f 
	" tag command
	:nmap <C-\><C-n> :tn<CR>
	:nmap <C-\><C-p> :tp<CR>
        " mouse event
	set mouse=a
        :nmap <2-LeftMouse> :cs find d <C-R>=expand("<cword>")<CR>:<C-R>=line('.')<CR>:%<CR>
    endif
    let loaded_gtags_cscope = 1
endfunction

if GtagsCscope_Auto_Load == 1
    call s:GtagsCscope()
endif
command! -nargs=0 GtagsCscope call s:GtagsCscope()
