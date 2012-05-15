var
  IniFile: TIniFile;

procedure StripComment(var s: String);
var
  Idx: PtrUInt;
  t: String;
begin
  Idx := Pos(CommentSeparator, s);
  if Idx <> 0 then
  begin
    if Idx > 2 then
    begin
      WriteStr(s, s[1..Idx - 2])
    end  
    else
    begin
      s := '';
    end;
  end;
end;

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
  Res: String;
  Sett: TRSetting;
  v: String;
begin
  Index := IndexOf(Setting);
  if Index <> -1 then
  begin
    Sett := PRSetting(FSettings^.GetNth(Index))^;
    WriteStr(v, Ord(Sett.ValueBoolean));
    Res := IniFile.ReadString(Sett.Section, Sett.Name, v);
    StripComment(Res);
    GetBoolean := Res <> '0';
  end;
end;

function TRSettings.GetInteger(const Setting: String): integer;
var
  i: integer;
  Index: TRSetIndex;
  Res: String;
  Sett: TRSetting;
  v: String;
begin
  Index := IndexOf(Setting);
  if Index <> -1 then
  begin
    Sett := PRSetting(FSettings^.GetNth(Index))^;
    WriteStr(v, Sett.ValueInteger);
    Res := IniFile.ReadString(Sett.Section, Sett.Name, v);
    ReadStr(Res, i);
    GetInteger := i;
  end;
end;

function TRSettings.GetString(const Setting: String): String;
var
  Index: TRSetIndex;
  Res: String;
  Sett: TRSetting;
begin
  Index := IndexOf(Setting);
  if Index <> -1 then
  begin
    Sett := PRSetting(FSettings^.GetNth(Index))^;
    Res := IniFile.ReadString(Sett.Section, Sett.Name, Sett.ValueString);
    StripComment(Res);
    GetString := Res;
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
  s, v: String;
begin
  New(ASetting);
  with ASetting^ do
  begin
    Name := Setting;
    Section := ASection;
    WriteStr(v, Ord(Value));
    s := IniFile.ReadString(ASection, Setting, v);
    StripComment(s);
    ValueBoolean := s <> '0';
    ValueType := TypeBoolean;
  end;

  ASetting^.Description := ADescription;
  FSettings^.Add(ASetting);
end;

procedure TRSettings.CheckInteger(const Setting, ASection: String; const Value: integer; const ADescription: String);
var
  ASetting: PRSetting;
  s, v: String;
begin
  New(ASetting);
  with ASetting^ do
  begin
    Name := Setting;
    Section := ASection;
    WriteStr(v, Value);
    s := IniFile.ReadString(ASection, Setting, v);
    StripComment(s);
    ReadStr(s, ValueInteger);
    ValueType := TypeInteger;
  end;

  ASetting^.Description := ADescription;
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
    StripComment(ValueString);
    ValueType := TypeString;
  end;

  ASetting^.Description := ADescription;
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
