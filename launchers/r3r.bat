@echo off

SET PHP_GTK_MAJOR=1

if "%PHP%" == "" goto SETPHP

goto TESTOPTS

:SETPHP
SET PHP=php

:TESTOPTS
if "%PHPOPTS%" == "" goto SETOPTS

goto TESTHOME

:SETOPTS
SET PHPOPTS=-q

:TESTHOME
if "%HOME%" == "" goto TESTAD

goto TESTLANG

:TESTAD
if "%APPDATA%" == "" goto SETAD

goto SETHOME

:SETAD
if "%USERNAME%" == "" goto SETUSER

goto SETDATA

:SETUSER
SET USERNAME="All Users"

:SETDATA
SET APPDATA="%WINDIR%\profiles\%USERNAME%\Application Data"

:SETHOME
SET HOME=%APPDATA%

if exist %HOME% goto TESTLANG

mkdir -p "%HOME%"

:TESTLANG
if "%LANG%" == "" goto SETLANG

goto STARTPROG

:SETLANG
SET LANG=en_US

:STARTPROG
%PHP% %PHPOPTS% r3r.php %1 %2 %3 %4 %5 %6 %7 %8 %9
