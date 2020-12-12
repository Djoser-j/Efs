@echo off
:: ---------------------------------------------------------------
:: Run Egypt_fr.exe

:: To be run in the Egyptfrac base directory
:: ---------------------------------------------------------------

:: demo to run
set file=Egypt_fr

echo.
if not exist %file%.exe goto :fail

echo  running %file%.exe

%file%.exe <%file% >%file%.log

echo.
exit

:fail
echo  %file%.exe not found
echo.
pause
