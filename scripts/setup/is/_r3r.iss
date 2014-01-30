; vim:nospell

#define ITDRoot ReadReg(HKEY_LOCAL_MACHINE, 'Software\Sherlock Software\InnoTools\Downloader', 'InstallPath', '')
#include ReadReg(HKEY_LOCAL_MACHINE, 'Software\Sherlock Software\InnoTools\Downloader', 'ScriptPath', '')

#define R3R_@UI@

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

[Components]
Name: "prog"; Description: "R3R (@UI@)"; Types: full compact custom; Flags: fixed
Name: "lib"; Description: "{cm:LibDesc}"; Types: full custom
Name: "i18n"; Description: "{cm:IntlDesc}"; Types: full custom
Name: "dev"; Description: "{cm:DevDesc}"; Types: full custom
#ifdef R3R_WX
Name: "wx"; Description: "{cm:wxDesc}"; Types: full custom
#endif

[Files]
Source: "r3r-@UI@.exe"; DestDir: "{app}\bin"; Components: prog
Source: "icons\r3r.ico"; DestDir: "{app}\share\icons"; Components: prog
Source: "src\utils\opml\*.bat"; DestDir: "{app}\bin"; Components: prog
Source: "src\utils\conv\r3r-conv.exe"; DestDir: "{app}\bin"; Components: prog
Source: "src\utils\opml\r3r_opml.exe"; DestDir: "{app}\bin"; Components: prog
#ifdef R3R_WX
Source: "src\libr3r\libr3r_shared.dll"; DestDir: "{app}\bin"; Components: prog
#else
Source: "src\libr3r\libr3r_shared.dll"; DestDir: "{app}\bin"; Components: dev
#endif
Source: "src\libr3r\libr3r.h"; DestDir: "{app}\include"; Components: dev

Source: "src/libr3r/po/de.mo"; DestDir: "{app}\share\locale\de\LC_MESSAGES"; DestName: "libr3r.mo"; Components: i18n
Source: "src/libr3r/po/en.mo"; DestDir: "{app}\share\locale\en\LC_MESSAGES"; DestName: "libr3r.mo"; Components: i18n
Source: "src/libr3r/po/eo.mo"; DestDir: "{app}\share\locale\eo\LC_MESSAGES"; DestName: "libr3r.mo"; Components: i18n
Source: "src/libr3r/po/es.mo"; DestDir: "{app}\share\locale\es\LC_MESSAGES"; DestName: "libr3r.mo"; Components: i18n

Source: "src/ui/@UI@/po/de.mo"; DestDir: "{app}\share\locale\de\LC_MESSAGES"; DestName: "r3r_@UI@.mo"; Components: i18n
Source: "src/ui/@UI@/po/en.mo"; DestDir: "{app}\share\locale\en\LC_MESSAGES"; DestName: "r3r_tui.mo"; Components: i18n
Source: "src/ui/@UI@/po/eo.mo"; DestDir: "{app}\share\locale\eo\LC_MESSAGES"; DestName: "r3r_@UI@.mo"; Components: i18n
Source: "src/ui/@UI@/po/es.mo"; DestDir: "{app}\share\locale\es\LC_MESSAGES"; DestName: "r3r_@UI@.mo"; Components: i18n

Source: "src/utils/conv/po/de.mo"; DestDir: "{app}\share\locale\de\LC_MESSAGES"; DestName: "r3r_conv.mo"; Components: i18n
Source: "src/utils/conv/po/en.mo"; DestDir: "{app}\share\locale\en\LC_MESSAGES"; DestName: "r3r_conv.mo"; Components: i18n
Source: "src/utils/conv/po/eo.mo"; DestDir: "{app}\share\locale\eo\LC_MESSAGES"; DestName: "r3r_conv.mo"; Components: i18n
Source: "src/utils/conv/po/es.mo"; DestDir: "{app}\share\locale\es\LC_MESSAGES"; DestName: "r3r_conv.mo"; Components: i18n

Source: "src/utils/opml/po/de.mo"; DestDir: "{app}\share\locale\de\LC_MESSAGES"; DestName: "r3r_opml.mo"; Components: i18n
Source: "src/utils/opml/po/en.mo"; DestDir: "{app}\share\locale\en\LC_MESSAGES"; DestName: "r3r_opml.mo"; Components: i18n
Source: "src/utils/opml/po/eo.mo"; DestDir: "{app}\share\locale\eo\LC_MESSAGES"; DestName: "r3r_opml.mo"; Components: i18n
Source: "src/utils/opml/po/es.mo"; DestDir: "{app}\share\locale\es\LC_MESSAGES"; DestName: "r3r_opml.mo"; Components: i18n

#ifdef R3R_TUI
Source: "src/ui/tui/docs/*.html";  DestDir: "{app}\share\r3r\docs"; Components: prog
Source: "src/ui/tui/skins/*.skin"; DestDir: "{app}\share\r3r\skins"; Components: prog
#endif

#ifdef R3R_WX
Source: "icons\r3r.png"; DestDir: "{app}\share\icons"; Components: prog
Source: "../wxwidgets/*.dll"; DestDir: "{app}\bin"; Components: wx
#endif

Source: "{#ITDROOT}\languages\*.ini"; Flags: dontcopy
Source: "scripts\setup\is\*.ini"; Flags: dontcopy

[Icons]
Name: "{userdesktop}\R3R"; FileName: "{app}\bin\r3r-@UI@.exe"; IconFilename: "{app}\share\icons\r3r.ico"; Tasks: desktopicon
Name: "{group}\R3R"; FileName: "{app}\bin\r3r-@UI@.exe"; IconFilename: "{app}\share\icons\r3r.ico"
Name: "{group}\Uninstall"; FileName: "{uninstallexe}"

[Languages]
Name: "de"; MessagesFile: "compiler:Languages\German.isl"; LicenseFile: "doc/deutsch/LIZENZ"
Name: "en"; MessagesFile: "compiler:Default.isl"; LicenseFile: "doc/english/LICENSE"
Name: "eo"; MessagesFile: "scripts/setup/is/Esperanto.isl"; LicenseFile: "doc/esperanto/PERMESILO"
Name: "es"; MessagesFile: "compiler:Languages\Spanish.isl"; LicenseFile: "doc/espanol/LICENCIA"

[CustomMessages]
de.CreateDesktopIcons=Erstellen &Desktop-Ikonen
en.CreateDesktopIcons=Create &desktop icons
eo.CreateDesktopIcons=Krei &labortablajn piktogramojn
es.CreateDesktopIcons=Crear pictogramas de &escritorio

de.libdesc=Kernbibliotheke
en.libdesc=Core Libraries
eo.libdesc=Kernaj bibliotekoj
es.libdesc=Bibliotecas principales

de.intldesc=Übersetzungen
en.intldesc=Translations
eo.intldesc=Tradukoj
es.intldesc=Traducciones

de.devdesc=Entwickler-Dateien
en.devdesc=Developer Files
eo.devdesc=Programistaj dosieroj
es.devdesc=Archivos para desarrollo de programas propios

#ifdef R3R_WX
de.wxdesc=wxWidgets-Bibliotheke
en.wxdesc=wxWidgets
eo.wxdesc=wxWidgets-bibliotekoj
es.wxdesc=Bibliotecas para wxWidgets
#endif

[Registry]
Root: HKCU; SubKey: "Environment"; ValueType: string; ValueName: "R3R_INSTALLED_PREFIX"; ValueData: "{app}"; Flags: uninsdeletevalue
Root: HKCU; Subkey: "Software\R3R"; Flags: uninsdeletekey
Root: HKCU; SubKey: "Software\R3R\Display"; ValueType: string; ValueName: "display-encoding"; ValueData: "ISO-8859-1"; Flags: createvalueifdoesntexist
Root: HKCU; SubKey: "Software\R3R\System"; ValueType: string; ValueName: "installed-prefix"; ValueData: "{app}"

[Code]
procedure InitializeWizard;
var
  LanguageFileName: String;
begin
  ITD_Init;

  try
    LanguageFileName := 'itd_' + ExpandConstant('{language}') + '.ini';
    ExtractTemporaryFile(LanguageFileName);
    ITD_LoadStrings(ExpandConstant('{tmp}') + '\' + LanguageFileName);
  except
  end;

  ITD_DownloadAfter(wpInstalling);
end;

procedure GetFile(DName, FName: String);
var
  post, rem: String;
begin
  try
    post := ExpandConstant('{app}') + '\bin\' + FName;
    rem := 'http://downloads.sourceforge.net/project/r3r/R3R%202.x%20for%20Windows%20(32-bit)/' + DName + '/' +  FName + '?r=&ts=0&use_mirror=auto';

    ITD_AddFile(rem, post)
  except
  end;
end;

procedure CurPageChanged(CurPageID: Integer);
begin
  if CurPageId = wpReady then
  begin
    if IsComponentSelected('lib') then
    begin
      GetFile('Core%20DLLs/20120422', 'intl.dll');
      GetFile('Core%20DLLs/20120422', 'libexpat-1.dll');
      GetFile('Core%20DLLs/20120422', 'libiconv2.dll');
      GetFile('Core%20DLLs/20120422', 'libidn-11.dll');
      GetFile('Core%20DLLs/20120422', 'libpcre-0.dll');
      GetFile('Core%20DLLs/20120422', 'readline5.dll');
    end
#ifdef R3R_WX
    else if IsComponentSelected('wx') then
    begin
      GetFile('wxWidgets%20DLLs/3.0.0', 'wxbase300u_gcc_custom.dll')
      GetFile('wxWidgets%20DLLs/3.0.0', 'wxmsw300u_core_gcc_custom.dll')
      GetFile('wxWidgets%20DLLs/3.0.0', 'wxmsw300u_html_gcc_custom.dll')
    end
#endif
  end
end;
