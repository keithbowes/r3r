var
  Reg: TRegistry;

const
  SectionPrefix = '\Software\R3R\';

constructor TRSettings.Create;
begin
  Reg := TRegistry.Create;
  Reg.RootKey := HKEY_CURRENT_USER;

  InitRec;
end;

destructor TRSettings.Destroy;
begin
  WriteRec;
  Reg.Free;
end;

function TRSettings.CheckBoolean(const Setting, ASection: String; const Value: Boolean): Boolean;
begin
  with FSettings[FRef] do
  begin
    Reg.OpenKey(SectionPrefix + ASection, true);
    Result := Reg.ValueExists(Setting);
    
    Name := Setting;
    Section := ASection;
    ValueType := TypeBoolean;

    if Result then
    begin
      ValueBoolean := Reg.ReadBool(Setting);
    end
    else
    begin
      ValueBoolean := Value;
    end;
  end;

  Reg.CloseKey;
  Inc(FRef);
end;

function TRSettings.CheckInteger(const Setting, ASection: String; const Value: integer): Boolean;
begin
  with FSettings[FRef] do
  begin
    Reg.OpenKey(SectionPrefix + ASection, true);
    Result := Reg.ValueExists(Setting);

    Name := Setting;
    Section := ASection;
    ValueType := TypeInteger;

    if Result then
    begin
      ValueInteger := Reg.ReadInteger(Setting);
    end
    else
    begin
      ValueInteger := Value;
    end;
  end;

  Reg.CloseKey;
  Inc(FRef);
end;

function TRSettings.CheckString(const Setting, ASection, Value: String): Boolean;
begin
  with FSettings[FRef] do
  begin
    Reg.OpenKey(SectionPrefix + ASection, true);
    Result := Reg.ValueExists(Setting);

    Name := Setting;
    Section := ASection;
    ValueString := Reg.ReadString(Setting);
    ValueType := TypeString;

    if Result then
    begin
      ValueString := Reg.ReadString(Setting);
    end
    else
    begin
      ValueString := Value;
    end;
  end;

  Reg.CloseKey;
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
    with FSettings[i], Reg do
    begin
      OpenKey(SectionPrefix + Section, true);

      case ValueType of
        TypeBoolean:
        begin
          WriteBool(Name, ValueBoolean);
        end;
        TypeInteger:
        begin
          WriteInteger(Name, ValueInteger);
        end;
        TypeString:
        begin
          if ValueString <> '' then
          begin
            WriteString(Name, ValueString);
          end;
        end;
      end;

      CloseKey;
    end;
  end;
end;
