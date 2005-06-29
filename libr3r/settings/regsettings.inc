var
  Reg: TRegistry;

const
  SectionPrefix = '\Software\R3R\';

constructor TRSettings.Create;
begin
  Reg := TRegistry.Create;
  Reg.RootKey := HKEY_CURRENT_USER;
end;

destructor TRSettings.Destroy;
begin
  Reg.CloseKey;
  Reg.Free;
end;

procedure TRSettings.SetSection(const Section: String);
begin
  if FCurrentSection <> Section then
  begin
    Reg.CloseKey;
    Reg.OpenKey(SectionPrefix + Section, true);
  end;

  FCurrentSection := Section;
end;

procedure TRSettings.SetString(const SettingName, SettingValue: String);
begin
  Reg.WriteString(SettingName, SettingValue);
end;

function TRSettings.GetString(const SettingName, Default: String): String;
begin
  Result := Reg.ReadString(SettingName);

  if Result = '' then
  begin
    Result := Default;
  end;
end;
