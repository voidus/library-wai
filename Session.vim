let SessionLoad = 1
let s:so_save = &g:so | let s:siso_save = &g:siso | setg so=0 siso=0 | setl so=-1 siso=-1
let v:this_session=expand("<sfile>:p")
silent only
silent tabonly
if expand('%') == '' && !&modified && line('$') <= 1 && getline(1) == ''
  let s:wipebuf = bufnr('%')
endif
let s:shortmess_save = &shortmess
set shortmess=aoO
badd +60 ~/Coding/library-wai/src/Ourstuff/Form.hs
badd +0 ~/Coding/library-wai/src/Ourstuff/ExistentialsExperimentials.hs
badd +34 ~/Coding/library-wai/tests/OurStuff/FormTest.hs
badd +77 ~/Coding/library-wai/ourstuff.cabal
argglobal
%argdel
edit ~/Coding/library-wai/src/Ourstuff/ExistentialsExperimentials.hs
let s:save_splitbelow = &splitbelow
let s:save_splitright = &splitright
set splitbelow splitright
wincmd _ | wincmd |
vsplit
1wincmd h
wincmd _ | wincmd |
split
1wincmd k
wincmd w
wincmd w
let &splitbelow = s:save_splitbelow
let &splitright = s:save_splitright
wincmd t
let s:save_winminheight = &winminheight
let s:save_winminwidth = &winminwidth
set winminheight=0
set winheight=1
set winminwidth=0
set winwidth=1
exe '1resize ' . ((&lines * 42 + 43) / 86)
exe 'vert 1resize ' . ((&columns * 149 + 149) / 299)
exe '2resize ' . ((&lines * 41 + 43) / 86)
exe 'vert 2resize ' . ((&columns * 149 + 149) / 299)
exe 'vert 3resize ' . ((&columns * 149 + 149) / 299)
argglobal
setlocal fdm=expr
setlocal fde=nvim_treesitter#foldexpr()
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=99
setlocal fml=1
setlocal fdn=20
setlocal fen
let s:l = 21 - ((9 * winheight(0) + 21) / 42)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 21
normal! 046|
lcd ~/Coding/library-wai
wincmd w
argglobal
if bufexists(fnamemodify("~/Coding/library-wai/src/Ourstuff/Form.hs", ":p")) | buffer ~/Coding/library-wai/src/Ourstuff/Form.hs | else | edit ~/Coding/library-wai/src/Ourstuff/Form.hs | endif
if &buftype ==# 'terminal'
  silent file ~/Coding/library-wai/src/Ourstuff/Form.hs
endif
balt ~/Coding/library-wai/src/Ourstuff/Form.hs
setlocal fdm=expr
setlocal fde=nvim_treesitter#foldexpr()
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=99
setlocal fml=1
setlocal fdn=20
setlocal fen
let s:l = 60 - ((31 * winheight(0) + 20) / 41)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 60
normal! 022|
wincmd w
argglobal
if bufexists(fnamemodify("~/Coding/library-wai/ourstuff.cabal", ":p")) | buffer ~/Coding/library-wai/ourstuff.cabal | else | edit ~/Coding/library-wai/ourstuff.cabal | endif
if &buftype ==# 'terminal'
  silent file ~/Coding/library-wai/ourstuff.cabal
endif
balt ~/Coding/library-wai/tests/OurStuff/FormTest.hs
setlocal fdm=expr
setlocal fde=nvim_treesitter#foldexpr()
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=99
setlocal fml=1
setlocal fdn=20
setlocal fen
let s:l = 77 - ((40 * winheight(0) + 42) / 84)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 77
normal! 015|
lcd ~/Coding/library-wai
wincmd w
3wincmd w
exe '1resize ' . ((&lines * 42 + 43) / 86)
exe 'vert 1resize ' . ((&columns * 149 + 149) / 299)
exe '2resize ' . ((&lines * 41 + 43) / 86)
exe 'vert 2resize ' . ((&columns * 149 + 149) / 299)
exe 'vert 3resize ' . ((&columns * 149 + 149) / 299)
tabnext 1
if exists('s:wipebuf') && len(win_findbuf(s:wipebuf)) == 0 && getbufvar(s:wipebuf, '&buftype') isnot# 'terminal'
  silent exe 'bwipe ' . s:wipebuf
endif
unlet! s:wipebuf
set winheight=1 winwidth=20
let &shortmess = s:shortmess_save
let &winminheight = s:save_winminheight
let &winminwidth = s:save_winminwidth
let s:sx = expand("<sfile>:p:r")."x.vim"
if filereadable(s:sx)
  exe "source " . fnameescape(s:sx)
endif
let &g:so = s:so_save | let &g:siso = s:siso_save
set hlsearch
doautoall SessionLoadPost
unlet SessionLoad
" vim: set ft=vim :
