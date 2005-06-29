var
  IniFile: TIniFile;

constructor TRSettings.Create;
begin
  IniFile := TIniFile.Create(SettingsDir + 'r3rrc');
end;

destructor TRSettings.Destroy;
begin
  IniFile.Free;
end;

procedure TRSettings.SetSection(const Section: String);
begin
  FCurrentSection := Section;
end;

procedure TRSettings.SetString(const SettingName, SettingValue: String);
begin
  IniFile.WriteString(FCurrentSection, SettingName, SettingValue);
end;

function TRSettings.GetString(const SettingName, Default: String): String;
begin
  Result := IniFile.ReadString(FCurrentSection, SettingName, Default);
end;
