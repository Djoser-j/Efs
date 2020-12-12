@echo off
:: ---------------------------------------------------------------
:: Compile Egypt_fr.bas

:: To be run in the Egyptfrac base directory
:: ---------------------------------------------------------------

:: module to compile
set file=Egypt_fr

:: Set path to the freeBasic compiler:
set Fbas=".."
:: without closing backslash.

:: source files directory
set tool=.\modules

set opts= -s console -w pedantic

%Fbas%\fbc %opts% %tool%\%file%.bas >compile.log

move %tool%\%file%.exe . >nul

exit
