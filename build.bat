set EMACSDIR=c:\emacs

set EMACS=%EMACS%\bin\emacs.exe
set WIDGETDIR=%EMACS%\lisp
set GNUSDIR=%EMACS%\lisp

cd lisp
copy w3-cfg.nt w3-cfg.el
%EMACS% -batch -q -l ./docomp.el -f emacs-batch-build-autoloads . auto-autoloads.el
%EMACS% -batch -q -l ./docomp.el -f compile-it
cd ..
