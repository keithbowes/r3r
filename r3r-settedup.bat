@echo off

TITLE R3R - PHP-GTK Runtime Environment

SET PATH=gtk;%PATH%
SET PHP=php\php.exe
SET PHPOPTS=-c php\php.ini.gtk -q

call r3r.bat
