; This is an Inno Setup Setup script for Windows.
; Get Inno Setup from http://www.jrsoftware.org/isinfo.php.

[Setup]
AllowNoIcons=yes
AppName=R3R
AppPublisher=Zooplah
AppPublisherURL=http://sourceforge.net/projects/r3r/
AppSupportURL=http://sourceforge.net/forum/forum.php?forum_id=313224
AppVerName=R3R {%VERSION} 
DefaultDirName={pf}\R3R
DefaultGroupName=R3R
DisableStartupPrompt=yes
LicenseFile=Artistic

[Files]
; The following are the locations on my computer.  You will probably need to change these.
Source: "r3r-settedup.bat"; DestDir: "{app}"; Components: r3r
Source: "r3r.*"; DestDir: "{app}"; Components: r3r
Source: "feeds.txt"; DestDir: "{app}"; Components: r3r
Source: "icons\r3r.ico"; DestDir: "{app}"; Components: r3r
Source: "version.txt"; DestDir: "{app}"; Components: r3r
Source: "includes\*.php"; DestDir: "{app}\includes"; Components: r3r
Source: "l10n\*"; DestDir: "{app}\l10n"; Components: r3r
Source: "php.ini.gtk"; DestDir:"{app}\php"; Components: php

; Replace by your PHP locations
Source: "C:\Program Files\R3R\php\php.exe"; DestDir: "{app}\php"; Components: php
Source: "C:\Program Files\R3R\php\extensions\php_gtk.dll"; DestDir: "{app}\php\extensions"; Components: pg
Source: "C:\Program Files\R3R\php\extensions\php_gtk_*.dll"; DestDir: "{app}\php\extensions"; Components: pg

; Replace by your system locations
Source: "C:\WINDOWS\System\php4ts.dll"; DestDir: "{sys}"; Components: php
Source: "C:\Program Files\R3R\gtk\*.dll"; DestDir: "{app}\gtk"; Components: gtk

[Tasks]
Name: desktopicon; Description: "&Create a desktop icon"

[Components]
Name: "r3r"; Description: "R3R"; Flags: fixed; Types: full compact
Name: "php"; Description: "The PHP interpreter"; Types: full
Name: "pg"; Description: "The PHP-GTK extension"; Types: full
Name: "gtk"; Description: "The GTK+ dynamic libraries"; Types: full

[Icons]
Name: "{group}\R3R"; FileName: "{app}\r3r-settedup.bat"; WorkingDir: "{app}"; IconFilename: "{app}\r3r.ico"
Name: "{group}\Uninstall"; FileName: "{uninstallexe}"
Name: "{userdesktop}\R3R"; FileName: "{app}\r3r-settedup.bat"; WorkingDir: "{app}"; IconFilename: "{app}\r3r.ico"; Tasks: desktopicon

