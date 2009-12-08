[Setup]
AllowNoIcons=yes
AppName=R3R
AppPublisher=The R3R Developers
AppPublisherURL=http://sourceforge.net/projects/r3r/
AppSupportURL=http://sourceforge.net/forum/forum.php?forum_id=313224
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
Source: "src\third-party\expas\expat.dll"; DestDir: "{app}\bin"
Source: "*.dll"; DestDir: "{app}\bin"
;insert_next_lang

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


