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
      Result := GetEnv('WINDIR') + PathDelim + 'profiles' + PathDelim + GetEnv('USERNAME') + PathDelim + 'Application Data';
    end;
  end;

  CheckDir(Result);
end;

function GetSettingsDir(const Version: String = ''): String;
begin
  Result := GetHome + PathDelim + '.r3r';
  if Version <> '' then
  begin
    Result := Result + '-';
  end;

  Result := Result + Version;
  CheckDir(Result);

  Result := Result + PathDelim;
end;

procedure TRSettings.Import;
begin
end;
