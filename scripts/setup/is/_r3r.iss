[Setup]
AllowNoIcons=yes
AlwaysRestart=yes
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
Name: desktopicon; Description: "&Create desktop icons"

[Files]
Source: "../*.dll"; DestDir: "{app}\bin"
Source: "r3r-tui.exe"; DestDir: "{app}\bin"
Source: "r3r-wx.exe"; DestDir: "{app}\bin"
Source: "icons\r3r.ico"; DestDir: "{app}\share\icons"
Source: "icons\r3r.png"; DestDir: "{app}\share\icons"
Source: "src\utils\opml\*.bat"; DestDir: "{app}\bin"
Source: "src\utils\opml\r3r_opml.exe"; DestDir: "{app}\bin"
Source: "src\libr3r\libr3r_shared.dll"; DestDir: "{app}\bin"

Source: "src/libr3r/po/de.mo"; DestDir: "{app}\share\locale\de\LC_MESSAGES"; DestName: "libr3r.mo"
Source: "src/libr3r/po/en.mo"; DestDir: "{app}\share\locale\en\LC_MESSAGES"; DestName: "libr3r.mo"
Source: "src/libr3r/po/eo.mo"; DestDir: "{app}\share\locale\eo\LC_MESSAGES"; DestName: "libr3r.mo"
Source: "src/libr3r/po/es.mo"; DestDir: "{app}\share\locale\es\LC_MESSAGES"; DestName: "libr3r.mo"

Source: "src/ui/tui/po/de.mo"; DestDir: "{app}\share\locale\de\LC_MESSAGES"; DestName: "r3r_tui.mo"
Source: "src/ui/tui/po/en.mo"; DestDir: "{app}\share\locale\en\LC_MESSAGES"; DestName: "r3r_tui.mo"
Source: "src/ui/tui/po/eo.mo"; DestDir: "{app}\share\locale\eo\LC_MESSAGES"; DestName: "r3r_tui.mo"
Source: "src/ui/tui/po/es.mo"; DestDir: "{app}\share\locale\es\LC_MESSAGES"; DestName: "r3r_tui.mo"

Source: "src/ui/wx/po/de.mo"; DestDir: "{app}\share\locale\de\LC_MESSAGES"; DestName: "r3r_wx.mo"
Source: "src/ui/wx/po/en.mo"; DestDir: "{app}\share\locale\en\LC_MESSAGES"; DestName: "r3r_wx.mo"
Source: "src/ui/wx/po/eo.mo"; DestDir: "{app}\share\locale\eo\LC_MESSAGES"; DestName: "r3r_wx.mo"
Source: "src/ui/wx/po/es.mo"; DestDir: "{app}\share\locale\es\LC_MESSAGES"; DestName: "r3r_wx.mo"

Source: "src/utils/opml/po/de.mo"; DestDir: "{app}\share\locale\de\LC_MESSAGES"; DestName: "r3r_opml.mo"
Source: "src/utils/opml/po/en.mo"; DestDir: "{app}\share\locale\en\LC_MESSAGES"; DestName: "r3r_opml.mo"
Source: "src/utils/opml/po/eo.mo"; DestDir: "{app}\share\locale\eo\LC_MESSAGES"; DestName: "r3r_opml.mo"
Source: "src/utils/opml/po/es.mo"; DestDir: "{app}\share\locale\es\LC_MESSAGES"; DestName: "r3r_opml.mo"

Source: "src/ui/tui/docs/*.html";  DestDir: "{app}\share\r3r\docs"
Source: "src/ui/tui/skins/*.skin"; DestDir: "{app}\share\r3r\skins"

[Icons]
Name: "{userdesktop}\R3R (TUI)"; FileName: "{app}\bin\r3r-tui.exe"; IconFilename: "{app}\share\icons\r3r.ico"; Tasks: desktopicon
Name: "{userdesktop}\R3R (GUI)"; FileName: "{app}\bin\r3r-wx.exe"; IconFilename: "{app}\share\icons\r3r.ico"; Tasks: desktopicon
Name: "{group}\R3R (TUI)"; FileName: "{app}\bin\r3r-tui.exe"; IconFilename: "{app}\share\icons\r3r.ico"
Name: "{group}\R3R (GUI)"; FileName: "{app}\bin\r3r-wx.exe"; IconFilename: "{app}\share\icons\r3r.ico"
Name: "{group}\Uninstall"; FileName: "{uninstallexe}"

[Languages]
Name: "de"; MessagesFile: "compiler:Languages\German.isl"
Name: "en"; MessagesFile: "compiler:Default.isl"
Name: "es"; MessagesFile: "compiler:Languages\Spanish.isl"

[Registry]
Root: HKCU; SubKey: "Environment"; ValueType: string; ValueName: "R3R_INSTALLED_PREFIX"; ValueData: "{app}"; Flags: uninsdeletevalue
Root: HKCU; Subkey: "Software\R3R"; Flags: uninsdeletekey
Root: HKCU; SubKey: "Software\R3R\System"; ValueType: string; ValueName: "installed-prefix"; ValueData: "{app}"




