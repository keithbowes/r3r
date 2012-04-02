; vim:nospell
#include ReadReg(HKEY_LOCAL_MACHINE,'Software\Sherlock Software\InnoTools\Downloader','ScriptPath','')

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
Name: desktopicon; Description: "{cm:CreateDesktopIcons}"
Name: cordedlls; Description "{cm:InstallCoreDlls}"; BeforeInstall: UnzipComp('core-dlls', '20120202');
Name: wxdlls; Description "{cm:InstallWxDlls}; BeforeInstall: UnzipComp('wxWidgets', '2.9.3');

[Files]
Source: "r3r-tui.exe"; DestDir: "{app}\bin"
Source: "r3r-wx.exe"; DestDir: "{app}\bin"
Source: "icons\r3r.ico"; DestDir: "{app}\share\icons"
Source: "icons\r3r.png"; DestDir: "{app}\share\icons"
Source: "src\utils\opml\*.bat"; DestDir: "{app}\bin"
Source: "src\utils\opml\r3r_opml.exe"; DestDir: "{app}\bin"
Source: "src\libr3r\libr3r_shared.dll"; DestDir: "{app}\bin"
Source: "src\ui\wx\libr3r.h"; DestDir: "{app}\include"

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

Source: "scripts/setup/is/LEERME.TXT"; DestDir: "{app}\share\r3r\docs"
Source: "scripts/setup/is/LEGUMIN.TXT"; DestDir: "{app}\share\r3r\docs"
Source: "scripts/setup/is/LESENMICH.TXT"; DestDir: "{app}\share\r3r\docs"
Source: "scripts/setup/is/README.TXT"; DestDir: "{app}\share\r3r\docs"

[Icons]
Name: "{userdesktop}\R3R (TUI)"; FileName: "{app}\bin\r3r-tui.exe"; IconFilename: "{app}\share\icons\r3r.ico"; Tasks: desktopicon
Name: "{userdesktop}\R3R (GUI)"; FileName: "{app}\bin\r3r-wx.exe"; IconFilename: "{app}\share\icons\r3r.ico"; Tasks: desktopicon
Name: "{group}\R3R (TUI)"; FileName: "{app}\bin\r3r-tui.exe"; IconFilename: "{app}\share\icons\r3r.ico"
Name: "{group}\R3R (GUI)"; FileName: "{app}\bin\r3r-wx.exe"; IconFilename: "{app}\share\icons\r3r.ico"
Name: "{group}\Uninstall"; FileName: "{uninstallexe}"

[Languages]
Name: "de"; MessagesFile: "compiler:Languages\German.isl"
Name: "en"; MessagesFile: "compiler:Default.isl"
Name: "eo"; MessagesFile: "scripts/setup/is/Esperanto.isl"
Name: "es"; MessagesFile: "compiler:Languages\Spanish.isl"

[CustomMessages]
de.CreateDesktopIcons=Erstellen Desktop-Ikonen
en.CreateDesktopIcons=Create &desktop icons
eo.CreateDesktopIcons= Krei &labortablajn piktogramojn
es.CreateDesktopIcons=Crear pictogramas de &escritorio

de.readme=LESENMICH.TXT
en.readme=README.TXT
eo.readme=LEGUMIN.TXT
es.readme=LEERME.TXT

de.viewreadme=Die LESENMICH-Datei sehen
en.viewreadme=View the README file
eo.viewreadme=Vidi la LEGUMIN-dosieron
es.viewreadme=Ver el archivo de LEERME

[Registry]
Root: HKCU; SubKey: "Environment"; ValueType: string; ValueName: "R3R_INSTALLED_PREFIX"; ValueData: "{app}"; Flags: uninsdeletevalue
Root: HKCU; Subkey: "Software\R3R"; Flags: uninsdeletekey
Root: HKCU; SubKey: "Software\R3R\Display"; ValueType: string; ValueName: "display-encoding"; ValueData: "ISO-8859-1"; Flags: createvalueifdoesntexist
Root: HKCU; SubKey: "Software\R3R\System"; ValueType: string; ValueName: "installed-prefix"; ValueData: "{app}"

[Run]
FileName: "{app}\share\r3r\docs\{cm:readme}"; Description: "{cm:viewreadme}"; Verb: "open"; Flags: postinstall shellexec

[Code]
procedure InitializeWizard;
begin
  ITD_Init;
end;

procedure UnzipComp(Name, Version: String);
var
  files, fs: Variant;
  FullName, loc, rem, post: String;
begin
  FullName := Name + ' -' + Version;
  //Use ExpandConstant?
  loc := '{tmp}\' + FullName;
  rem := 'http://sourceforge.net/projects/r3r/files/' + Name + '/' +  FullName + '/download';
  post := '{app}\bin';
  ITD_DownFloadFile(rem, loc);
  fs := CreateOleObject('Shell.Application');
  files := fs.NameSpace(loc).Items;
  CreateDir(post);
  fss.NameSpace(post).CopyHere(files);
end;
