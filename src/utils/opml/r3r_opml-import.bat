REM Simple drag-and-drop batch file for easy importing

SET outfile=subscriptions.txt
echo Outputting to %outfile%

call r3r_opml 1 "%1" "%outfile%"
