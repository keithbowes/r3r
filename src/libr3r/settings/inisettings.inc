var
  IniFile: TIniFile;

constructor TRSettings.Create;
begin
  IniFile := TIniFile.Create(SettingsDir + 'r3r.ini');
  InitRec;
end;

destructor TRSettings.Destroy;
begin
  WriteRec;
  IniFile.Free;
end;

function TRSettings.CheckBoolean(const Setting, ASection: String; const Value: Boolean): Boolean;
begin
  Result := IniFile.ValueExists(ASection, Setting);

  with FSettings[FRef] do
  begin
    Name := Setting;
    Section := ASection;
    ValueBoolean := IniFile.ReadBool(ASection, Setting, Value);
    ValueType := TypeBoolean;
  end;

  Inc(FRef);
end;

function TRSettings.CheckInteger(const Setting, ASection: String; const Value: integer): Boolean;
begin
  Result := IniFile.ValueExists(ASection, Setting);

  with FSettings[FRef] do
  begin
    Name := Setting;
    Section := ASection;
    ValueInteger := IniFile.ReadInteger(ASection, Setting, Value);
    ValueType := TypeInteger;
  end;

  Inc(FRef);
end;

function TRSettings.CheckString(const Setting, ASection, Value: String): Boolean;
begin
  Result := IniFile.ValueExists(ASection, Setting);

  with FSettings[FRef] do
  begin
    Name := Setting;
    Section := ASection;
    ValueString := IniFile.ReadString(ASection, Setting, Value);
    ValueType := TypeString;
  end;

  Inc(FRef);
end;

function TRSettings.CleanRec: Boolean;
begin
  FRef := 1;
  Result := false;
end;

procedure TRSettings.ReadRec;
begin
end;

procedure TRSettings.WriteRec;
var
  i: byte;
begin
  for i := 1 to SettingsCount do
  begin
    with FSettings[i], IniFile do
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
  end;
end;
