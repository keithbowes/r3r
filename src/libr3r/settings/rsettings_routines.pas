unit RSettings_Routines;

interface

procedure CheckDir(const Dir: String);
function SettingsDir: String;

implementation

uses
{$IFNDEF FPC}
  Dos,
{$ENDIF}

{$IFDEF __GPC__}
  GPC,
{$ENDIF}
  SysUtils;

procedure CheckDir(const Dir: String);
begin
  if not DirectoryExists(Dir) then
  begin
    CreateDir(Dir);
  end;
end;

function SettingsDir: String;
{$IFDEF __GPC__}
const
  PathDelim = DirSeparator;
{$ENDIF}

var
  Ret: String;
begin
{$IFDEF FPC}
  Ret := GetAppConfigDir(false);
{$ELSE}
  Ret := GetEnv('HOME');

  if Ret = '' then
  begin
    Ret := GetEnv('USERPROFILE');
  end;

  Ret := Ret + PathDelim +  '.config' + PathDelim + 'r3r' + PathDelim;
{$ENDIF}

  CheckDir(Ret);
  SettingsDir := Ret;
end;

end.
