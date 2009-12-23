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
LicenseFile=doc\english\LICENSE
OutputDir=scripts\setup\is
SourceDir=..\..\..

[Tasks]
Name: desktopicon; Description: "&Create a desktop icon"

[Files]
Source: "r3r.exe"; DestDir: "{app}\bin";
Source: "icons\r3r.ico"; DestDir: "{app}\share\icons";
Source: "*.dll"; DestDir: "{app}\bin"

Source: "src/libr3r/po/de.mo"; DestDir: "{app}\share\locale\LC_MESSAGES\de\libr3r.mo"
Source: "src/libr3r/po/en.mo"; DestDir: "{app}\share\locale\LC_MESSAGES\en\libr3r.mo"
Source: "src/libr3r/po/eo.mo"; DestDir: "{app}\share\locale\LC_MESSAGES\eo\libr3r.mo"
Source: "src/libr3r/po/es.mo"; DestDir: "{app}\share\locale\LC_MESSAGES\es\libr3r.mo"

Source: "src/ui/@UI/po/de.mo"; DestDir: "{app}\share\locale\LC_MESSAGES\de\r3r_@UI@.mo"
Source: "src/ui/@UI/po/en.mo"; DestDir: "{app}\share\locale\LC_MESSAGES\en\r3r_@UI@.mo"
Source: "src/ui/@UI/po/eo.mo"; DestDir: "{app}\share\locale\LC_MESSAGES\eo\r3r_@UI@.mo"
Source: "src/ui/@UI/po/es.mo"; DestDir: "{app}\share\locale\LC_MESSAGES\es\r3r_@UI@.mo"

[Icons]
Name: "{userdesktop}\R3R"; FileName: "{app}\bin\r3r.exe"; IconFilename: "{app}\share\icons\r3r.ico"; Tasks: desktopicon
Name: "{group}\R3R"; FileName: "{app}\bin\r3r.exe"; IconFilename: "{app}\share\icons\r3r.ico"
Name: "{group}\Uninstall"; FileName: "{uninstallexe}"

[Languages]
Name: "de"; MessagesFile: "compiler:Languages\German.isl"; LicenseFile: "doc\deutsch\LIZENZ"
Name: "en"; MessagesFile: "compiler:Default.isl"
Name: "es"; MessagesFile: "compiler:Languages\Spanish.isl"

[Registry]
Root: HKCU; Subkey: "Software\R3R"; Flags: uninsdeletekey
