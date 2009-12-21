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

procedure TRSettings.CheckBoolean(const Setting, ASection: String; const Value: Boolean);
var
  ASetting: PRSetting;
begin
  New(ASetting);
  with ASetting^ do
  begin
    Name := Setting;
    Section := ASection;
    ValueBoolean := TabFile^.ReadBool(ASection, Setting, Value);
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
    ValueInteger := TabFile^.ReadInteger(ASection, Setting, Value);
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
