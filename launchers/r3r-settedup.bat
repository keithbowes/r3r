@echo off

TITLE R3R - PHP-GTK Runtime Environment

SET PATH=gtk;php;%PATH%
SET PHP=php.exe
SET PHPOPTS=-c php\php.ini-gtk -q

call r3r.bat
