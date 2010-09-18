REM Simple drag-and-drop batch file for easy exporting

SET outfile=subscriptionList.opml
echo Outputting to %outfile%

call r3r_opml 0 "%1" "%outfile%"
