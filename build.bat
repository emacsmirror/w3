@echo off

set FALLBACKEMACSDIR=c:\emacs

REM This allows the user to specify the emacs root on the command
REM line.
set COMMANDLINEGIVEN=yes
if "%1" == "" set COMMANDLINEGIVEN=no
if "%COMMANDLINEGIVEN%" == "yes" set EMACSDIR=%1

REM If no emacs directory is set in the default environment, use our
REM fallback.
if "%EMACSDIR%" == "" echo EMACSDIR not set!  Using %FALLBACKEMACSDIR% as a default...
if "%EMACSDIR%" == "" set EMACSDIR=%FALLBACKEMACSDIR%

set EMACS=%EMACSDIR%\bin\emacs.exe
set WIDGETDIR=%EMACSDIR%\lisp
set GNUSDIR=%EMACSDIR%\lisp

cd lisp
copy w3-cfg.nt w3-cfg.el
%EMACS% -batch -q -l ./docomp.el -f emacs-batch-build-autoloads . auto-autoloads.el
%EMACS% -batch -q -l ./docomp.el -f compile-it
cd ..
