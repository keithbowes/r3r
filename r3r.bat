@echo off

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

goto STARTPROG

:TESTAD
if "%USERPROFILE%" == "" goto SETUP

goto SETHOME

:SETAD
SET APPDATA=%WINDIR%\Profiles\%USERNAME%

:SETHOME
SET HOME=%APPDATA%

if exist %HOME% goto TESTLANG

mkdir -p "%HOME%"

:TESTLANG
if "%LANG%" = "" goto SETLANG

goto STARTPROG

:SETLANG
SET LANG=en_US

:STARTPROG
%PHP% %PHPOPTS% r3r.php %1 %2 %3 %4 %5 %6 %7 %8 %9
