unit RSettings_Routines;

interface

procedure CheckDir(const Dir: String);
function SettingsDir: String;

implementation

uses
  SysUtils;

procedure CheckDir(const Dir: String);
begin
  if not DirectoryExists(Dir) then
  begin
    CreateDir(Dir);
  end;
end;

function SettingsDir: String;
begin
  Result := GetAppConfigDir(false);
  CheckDir(Result);
end;

end.
