set EMACS=emacs

cd lisp
copy w3-cfg.nt w3-cfg.el
%EMACS% -batch -q -l ./docomp.el -f compile-it
