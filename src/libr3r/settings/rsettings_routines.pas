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
procedure SetInstalledPrefix(const prefix: String);

implementation

uses
  Dos, Info, SysUtils;

var
  PrivName: String;
  RPrefix: String;

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
    Ret := Ret + PathDelim + PrivName;
  end
  else
  begin
    Ret := SettingsDir + PathDelim + 'data';
  end;

  Ret := Ret + PathDelim;
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
    Ret := SettingsDir + PathDelim + 'cache';
  end;

  Ret := Ret + PathDelim;
  CheckDir(Ret);

  CacheDir := Ret;
end;

procedure CheckDir(const Dir: String);
begin
  if not DirectoryExists(Dir) then
  begin
    CreateDir(Dir);
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
  if RPrefix <> '' then
  begin
    Res := RPrefix
  end
  else
  begin
    Res := GetEnv('R3R_INSTALLED_PREFIX');

    if Res = '' then
    begin
      Res := InstalledPrefix
    end
  end;

  GetInstalledPrefix := Res
end;

procedure SetInstalledPrefix(const prefix: String);
begin
  RPrefix := prefix
end;

initialization

PrivName := LowerCase(AppName);
RPrefix := '';

end.
