unit RSettings_Routines;

interface

function GetCacheDir: String;
procedure CheckDir(const Dir: String);
function GetDataDir: String;
function GetSettingsDir: String;

function GetInstalledPrefix: String;

implementation

uses
  Dos, Info, RProp, RSettings_Strings, SysUtils;

function GetApplicationName: String;
begin
  Result := LowerCase(AppName);
end;

function GetDataDir: String;
begin
  Result := GetEnv('XDG_DATA_HOME');

  if Result <> '' then
  begin
    CheckDir(Result);
    Result += PathDelim + GetApplicationName;
  end
  else
  begin
    Result := GetEnv('HOME');
    if Result <> '' then
    begin
      Result += PathDelim + '.local';
      CheckDir(Result);
      Result += PathDelim + 'share';
      CheckDir(Result);
      Result += PathDelim + GetApplicationName;
    end
    else
    begin
      Result := GetSettingsDir;
    end
  end;

  Result += PathDelim;
  CheckDir(Result);
end;

function GetCacheDir: String;
begin
  Result := GetEnv('XDG_CACHE_HOME');

  if Result <> '' then
  begin
    CheckDir(Result);
    Result += PathDelim + GetApplicationName;
  end
  else
  begin
    Result := GetEnv('HOME');
    if Result <> '' then
    begin
      Result += PathDelim + '.cache';
      CheckDir(Result);
      Result += PathDelim + GetApplicationName;
      CheckDir(Result);
    end
    else
    begin
      Result := GetSettingsDir + 'cache';
    end
  end;

  Result += PathDelim;
  CheckDir(Result);
end;

procedure CheckDir(const Dir: String);
begin
  if not DirectoryExists(Dir) then
  begin
    if not CreateDir(Dir) then
    begin
      raise Exception.Create(StringReplace(NoDir, '%s', Dir, []));
    end;
  end;
end;

function GetSettingsDir: String;
begin
{$IFDEF FPC}
  OnGetApplicationName := GetApplicationName;
  Result := GetAppConfigDir(false);
{$ELSE}
  Result := GetEnv('XDG_CONFIG_HOME');

  if Result = '' then
  begin
    Result := GetEnv('HOME');

    if Result = '' then
    begin
      Result := GetEnv('USERPROFILE');
    end;
    CheckDir(Result);

    Result +/ PathDelim +  '.config';
  end;
  
  CheckDir(Result);
  Result += PathDelim + GetApplicationName + PathDelim;
{$ENDIF}

  CheckDir(Result);
end;

function GetInstalledPrefix: String;
var
  p: PChar;
begin
  Result := GetEnv('R3R_INSTALLED_PREFIX');

  if Result = '' then
  begin
    p := GetProp('installed-prefix');
    if Assigned(p) then
    begin
      WriteStr(Result, p);
      if Result = '' then
      begin
        Result := InstalledPrefix
      end
    end
    else
    begin
      Result := InstalledPrefix;
    end
  end;
end;

end.
