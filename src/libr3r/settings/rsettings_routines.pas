unit RSettings_Routines;

interface

{$IFDEF __GPC__}
uses
  GPC;

const
  PathDelim = DirSeparator;
{$ENDIF}

function CacheDir: String;
procedure CheckDir(const Dir: String);
function DataDir: String;
function SettingsDir: String;

function GetInstalledPrefix: String;

implementation

uses
  Dos, Info, RProp, RSettings_Strings, SysUtils;

var
  PrivName: String;

function GetApplicationName: String;
begin
  GetApplicationName := LowerCase(PrivName);
end;

function DataDir: String;
var
  Ret: String;
begin
  Ret := GetEnv('XDG_DATA_HOME');

  if Ret <> '' then
  begin
    CheckDir(Ret);
    Ret := Ret + PathDelim + PrivName + PathDelim;
  end
  else
  begin
    Ret := SettingsDir;
  end;

  CheckDir(Ret);
  DataDir := Ret;
end;

function CacheDir: String;
var
  Ret: String;
begin
  Ret := GetEnv('XDG_CACHE_HOME');

  if Ret <> '' then
  begin
    CheckDir(Ret);
    Ret := Ret + PathDelim + PrivName;
  end
  else
  begin
    Ret := SettingsDir + 'cache';
  end;

  Ret := Ret + PathDelim;
  CheckDir(Ret);

  CacheDir := Ret;
end;

procedure CheckDir(const Dir: String);
begin
  if not DirectoryExists(Dir) then
  begin
    if not CreateDir(Dir) then
    begin
      WriteLn(StringReplace(NoDir, '%s', Dir, []));
      Halt(Random(256));
    end;
  end;
end;

function SettingsDir: String;
var
  Ret: String;
begin
{$IFDEF FPC}
  OnGetApplicationName := GetApplicationName;
  Ret := GetAppConfigDir(false);
{$ELSE}
  Ret := GetEnv('XDG_CONFIG_HOME');

  if Ret = '' then
  begin
    Ret := GetEnv('HOME');

    if Ret = '' then
    begin
      Ret := GetEnv('USERPROFILE');
    end;
    CheckDir(Ret);

    Ret := Ret + PathDelim +  '.config';
  end;
  
  CheckDir(Ret);
  Ret := Ret + PathDelim + PrivName + PathDelim;
{$ENDIF}

  CheckDir(Ret);
  SettingsDir := Ret;
end;

function GetInstalledPrefix: String;
var
  Res: String;
begin
  Res := GetEnv('R3R_INSTALLED_PREFIX');

  if Res = '' then
  begin
    Res := StrPas(GetProp('installed-prefix'));
    if Res = '' then
    begin
      Res := InstalledPrefix
    end
  end;

  GetInstalledPrefix := Res
end;

initialization

PrivName := LowerCase(AppName);

end.
