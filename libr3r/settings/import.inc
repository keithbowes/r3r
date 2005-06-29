procedure CheckDir(const Dir: String);
begin
  if not DirectoryExists(Dir) then
  begin
    MkDir(Dir);
  end;
end;

function GetHome: String;
begin
  Result := GetEnv('HOME');
  if Result = '' then
  begin
    Result := GetEnv('APPDATA');
    if Result = '' then
    begin
      Result := GetEnv('WINDIR') + '/profiles/' + GetEnv('USERNAME');
    end;
  end;

  CheckDir(Result);
end;

function GetSettingsDir: String;
begin
  Result := GetHome + '/.r3r-2.0/';
  CheckDir(Result);
end;

procedure TRSettings.Import;
begin
end;
