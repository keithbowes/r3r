var
  IniFile: TIniFile;

constructor TRSettings.Create;
begin
  IniFile := TIniFile.Create(SettingsDir + 'r3r.ini');
  New(FSettings, Init);

  InitRec;
end;

destructor TRSettings.Destroy;
begin
  WriteRec;

  Dispose(FSettings, Done);
  IniFile.Free;
end;

function TRSettings.GetBoolean(const Index: TRSetIndex): Boolean;
var
  Sett: TRSetting;
begin
  if Index <> -1 then
  begin
    Sett := PRSetting(FSettings^.GetNth(Index))^;
    GetBoolean := IniFile.ReadBool(Sett.Section, Sett.Name, Sett.ValueBoolean);
  end;
end;

function TRSettings.GetInteger(const Index: TRSetIndex): integer;
var
  Sett: TRSetting;
begin
  if Index <> -1 then
  begin
    Sett := PRSetting(FSettings^.GetNth(Index))^;
    GetInteger := IniFile.ReadInteger(Sett.Section, Sett.Name, Sett.ValueInteger);
  end;
end;

function TRSettings.GetString(const Index: TRSetIndex): String;
var
  Sett: TRSetting;
begin
  if Index <> -1 then
  begin
    Sett := PRSetting(FSettings^.GetNth(Index))^;
    GetString := IniFile.ReadString(Sett.Section, Sett.Name, Sett.ValueString);
  end;
end;

procedure TRSettings.SetBoolean(const Index: TRSetIndex; const Setting: Boolean);
begin
  if Index <> -1 then
  begin
    with PRSetting(FSettings^.GetNth(Index))^ do
    begin
      IniFile.WriteBool(Section, Name, Setting);
    end;
  end;
end;

procedure TRSettings.SetInteger(const Index: TRSetIndex; const Setting: integer);
begin
  if Index <> -1 then
  begin
    with PRSetting(FSettings^.GetNth(Index))^ do
    begin
      IniFile.WriteInteger(Section, Name, Setting);
    end;
  end;
end;

procedure TRSettings.SetString(const Index: TRSetIndex; const Setting: String);
begin
  if Index <> -1 then
  begin
    with PRSetting(FSettings^.GetNth(Index))^ do
    begin
      IniFile.WriteString(Section, Name, Setting);
    end;
  end;
end;

procedure TRSettings.CheckBoolean(const Setting, ASection: String; const Value: Boolean);
var
  ASetting: PRSetting;
begin
  New(ASetting);
  with ASetting^ do
  begin
    Name := Setting;
    Section := ASection;
    ValueBoolean := IniFile.ReadBool(ASection, Setting, Value);
    ValueType := TypeBoolean;
  end;
  FSettings^.Add(ASetting);
end;

procedure TRSettings.CheckInteger(const Setting, ASection: String; const Value: integer);
var
  ASetting: PRSetting;
begin
  New(ASetting);
  with ASetting^ do
  begin
    Name := Setting;
    Section := ASection;
    ValueInteger := IniFile.ReadInteger(ASection, Setting, Value);
    ValueType := TypeInteger;
  end;
  FSettings^.Add(ASetting);
end;

procedure TRSettings.CheckString(const Setting, ASection, Value: String);
var
  ASetting: PRSetting;
begin
  New(ASetting);
  with ASetting^ do
  begin
    Name := Setting;
    Section := ASection;
    ValueString := IniFile.ReadString(ASection, Setting, Value);
    ValueType := TypeString;
  end;
  FSettings^.Add(ASetting);
end;

procedure TRSettings.ReadRec;
begin
end;

procedure TRSettings.WriteRec;
var
  ASetting: PRSetting;
  i: byte;
begin
  for i := 0 to FSettings^.Count - 1 do
  begin
    ASetting := PRSetting(FSettings^.GetNth(i));
    Dispose(ASetting);
  end;
end;
