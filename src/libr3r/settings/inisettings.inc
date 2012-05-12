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

function TRSettings.GetBoolean(const Setting: String): Boolean;
var
	Index: TRSetIndex;
  Sett: TRSetting;
begin
	Index := IndexOf(Setting);
  if Index <> -1 then
  begin
    Sett := PRSetting(FSettings^.GetNth(Index))^;
    GetBoolean := IniFile.ReadBool(Sett.Section, Sett.Name, Sett.ValueBoolean);
  end;
end;

function TRSettings.GetInteger(const Setting: String): integer;
var
	Index: TRSetIndex;
  Sett: TRSetting;
begin
	Index := IndexOf(Setting);
  if Index <> -1 then
  begin
    Sett := PRSetting(FSettings^.GetNth(Index))^;
    GetInteger := IniFile.ReadInteger(Sett.Section, Sett.Name, Sett.ValueInteger);
  end;
end;

function TRSettings.GetString(const Setting: String): String;
var
	Index: TRSetIndex;
  Sett: TRSetting;
begin
	Index := IndexOf(Setting);
  if Index <> -1 then
  begin
    Sett := PRSetting(FSettings^.GetNth(Index))^;
    GetString := IniFile.ReadString(Sett.Section, Sett.Name, Sett.ValueString);
  end;
end;

procedure TRSettings.SetBoolean(const Setting: String; const Value: Boolean);
var
	Index: TRSetIndex;
begin
	Index := IndexOf(Setting);
  if Index <> -1 then
  begin
    with PRSetting(FSettings^.GetNth(Index))^ do
    begin
      IniFile.WriteBool(Section, Name, Value);
			ValueBoolean := Value;
    end;
  end;
end;

procedure TRSettings.SetInteger(const Setting: String; const Value: integer);
var
	Index: TRSetIndex;
begin
	Index := IndexOf(Setting);
  if Index <> -1 then
  begin
    with PRSetting(FSettings^.GetNth(Index))^ do
    begin
      IniFile.WriteInteger(Section, Name, Value);
			ValueInteger := Value;
    end;
  end;
end;

procedure TRSettings.SetString(const Setting: String; const Value: String);
var
	Index: TRSetIndex;
begin
	Index := IndexOf(Setting);
  if Index <> -1 then
  begin
    with PRSetting(FSettings^.GetNth(Index))^ do
    begin
      IniFile.WriteString(Section, Name, Value);
			ValueString := Value;
    end;
  end;
end;

procedure TRSettings.CheckBoolean(const Setting, ASection: String; const Value: Boolean; const ADescription: String);
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

  ASetting.Description := ADescription;
  FSettings^.Add(ASetting);
end;

procedure TRSettings.CheckInteger(const Setting, ASection: String; const Value: integer; const ADescription: String);
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

  ASetting.Description := ADescription;
  FSettings^.Add(ASetting);
end;

procedure TRSettings.CheckString(const Setting, ASection, Value, ADescription: String);
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

  ASetting.Description := ADescription;
  FSettings^.Add(ASetting);
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

{
  vi:filetype=pascal
}
