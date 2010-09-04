REM Simple drag-and-drop batch file for easy exporting

SET outfile=%USERPROFILE%\Local Settings\Application Data\r3r\subscriptionList.opml
echo Outputting to %outfile%

call r3r_opml 0 "%1" "%outfile%"
