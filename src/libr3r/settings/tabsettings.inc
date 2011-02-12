var
  TabFile: PTabFile;

constructor TRSettings.Create;
begin
  New(TabFile, Init(SettingsDir + 'r3r.tab'));
  New(FSettings, Init);

  InitRec;
end;

destructor TRSettings.Destroy;
begin
  WriteRec;

  Dispose(FSettings, Done);
  Dispose(TabFile, Done);
end;

function TRSettings.GetBoolean(const Index: TRSetIndex): Boolean;
begin
  if Index <> -1 then
  begin
    GetBoolean := PRSetting(FSettings^.GetNth(Index))^.ValueBoolean;
  end;
end;

function TRSettings.GetInteger(const Index: TRSetIndex): integer;
begin
  if Index <> -1 then
  begin
    GetInteger := PRSetting(FSettings^.GetNth(Index))^.ValueInteger;
  end;
end;

function TRSettings.GetString(const Index: TRSetIndex): String;
begin
  if Index <> -1 then
  begin
    GetString := PRSetting(FSettings^.GetNth(Index))^.ValueString;
  end;
end;

procedure TRSettings.SetBoolean(const Index: TRSetIndex; const Setting: Boolean);
begin
  if Index <> -1 then
  begin
    PRSetting(FSettings^.GetNth(Index))^.ValueBoolean := Setting;
  end;
end;

procedure TRSettings.SetInteger(const Index: TRSetIndex; const Setting: integer);
begin
  if Index <> -1 then
  begin
    PRSetting(FSettings^.GetNth(Index))^.ValueInteger := Setting;
  end;
end;

procedure TRSettings.SetString(const Index: TRSetIndex; const Setting: String);
begin
  if Index <> -1 then
  begin
    PRSetting(FSettings^.GetNth(Index))^.ValueString := Setting;
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
    Description := ADescription;
    ValueBoolean := TabFile^.ReadBool(ASection, Setting, Value);
    ValueType := TypeBoolean;
  end;
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
    Description := ADescription;
    ValueInteger := TabFile^.ReadInteger(ASection, Setting, Value);
    ValueType := TypeInteger;
  end;
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
    Description := ADescription;
    ValueString := TabFile^.ReadString(ASection, Setting, Value);
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
  TabFile^.BeginWrite;
  for i := 0 to FSettings^.Count - 1 do
  begin
    ASetting := PRSetting(FSettings^.GetNth(i));

    with ASetting^, TabFile^ do
    begin
      case ValueType of
        TypeBoolean:
        begin
          WriteBool(Section, Name, ValueBoolean);
        end;
        TypeInteger:
        begin
          WriteInteger(Section, Name, ValueInteger);
        end;
        TypeString:
        begin
          WriteString(Section, Name, ValueString);
        end;
      end;
    end;

    Dispose(ASetting);
  end;

  TabFile^.EndWrite;
end;

{
  vim:filetype=pascal
}
