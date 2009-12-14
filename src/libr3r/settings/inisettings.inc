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

function TRSettings.CheckBoolean(const Setting, ASection: String; const Value: Boolean): Boolean;
var
  ASetting: PRSetting;
begin
  Result := IniFile.ValueExists(ASection, Setting);

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

function TRSettings.CheckInteger(const Setting, ASection: String; const Value: integer): Boolean;
var
  ASetting: PRSetting;
begin
  Result := IniFile.ValueExists(ASection, Setting);

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

function TRSettings.CheckString(const Setting, ASection, Value: String): Boolean;
var
  ASetting: PRSetting;
begin
  Result := IniFile.ValueExists(ASection, Setting);

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

    with ASetting^, IniFile do
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
end;
