[Setup]
AllowNoIcons=yes
AppName=R3R
AppPublisher=Keith Bowes
AppPublisherURL=http://sourceforge.net/users/zooplah
AppSupportURL=http://sourceforge.net/forum/forum.php?forum_id=313224
AppUpdatesURL=http://sourceforge.net/projects/r3r/
AppVerName=R3R @VERSION@ 
DefaultDirName={pf}\R3R 2.0
DefaultGroupName=R3R 2.0
DisableStartupPrompt=yes
OutputDir=scripts\setup\is
SourceDir=..\..\..

[Tasks]
Name: desktopicon; Description: "&Create a desktop icon"

[Files]
Source: "*.dll"; DestDir: "{app}\bin"
Source: "r3r.exe"; DestDir: "{app}\bin";
Source: "icons\r3r.ico"; DestDir: "{app}\share\icons";
Source: "src\util\opml\*.bat"; DestDir: "{app}\bin"
Source: "src\util\opml\r3r_opml.exe"; DestDir: "{app}\bin"
Source: "src\libr3r\libr3r_shared.dll"; DestDir: "{app}\bin"

Source: "src/libr3r/po/de.mo"; DestDir: "{app}\share\locale\LC_MESSAGES\de"; DestName: "libr3r.mo"
Source: "src/libr3r/po/en.mo"; DestDir: "{app}\share\locale\LC_MESSAGES\en"; DestName: "libr3r.mo"
Source: "src/libr3r/po/eo.mo"; DestDir: "{app}\share\locale\LC_MESSAGES\eo"; DestName: "libr3r.mo"
Source: "src/libr3r/po/es.mo"; DestDir: "{app}\share\locale\LC_MESSAGES\es"; DestName: "libr3r.mo"

Source: "src/ui/@UI@/po/de.mo"; DestDir: "{app}\share\locale\LC_MESSAGES\de"; DestName: "r3r_@UI@.mo"
Source: "src/ui/@UI@/po/en.mo"; DestDir: "{app}\share\locale\LC_MESSAGES\en"; DestName: "r3r_@UI@.mo"
Source: "src/ui/@UI@/po/eo.mo"; DestDir: "{app}\share\locale\LC_MESSAGES\eo"; DestName: "r3r_@UI@.mo"
Source: "src/ui/@UI@/po/es.mo"; DestDir: "{app}\share\locale\LC_MESSAGES\es"; DestName: "r3r_@UI@.mo"

[Icons]
Name: "{userdesktop}\R3R"; FileName: "{app}\bin\r3r.exe"; IconFilename: "{app}\share\icons\r3r.ico"; Tasks: desktopicon
Name: "{group}\R3R"; FileName: "{app}\bin\r3r.exe"; IconFilename: "{app}\share\icons\r3r.ico"
Name: "{group}\Uninstall"; FileName: "{uninstallexe}"

[Languages]
Name: "de"; MessagesFile: "compiler:Languages\German.isl"
Name: "en"; MessagesFile: "compiler:Default.isl"
Name: "es"; MessagesFile: "compiler:Languages\Spanish.isl"

[Registry]
Root: HKCU; SubKey: "Software\R3R\System"; ValueType: string; ValueName: "installed-prefix"; ValueData: "{app}"
Root: HKCU; Subkey: "Software\R3R"; Flags: uninsdeletekey




