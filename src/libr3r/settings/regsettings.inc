var
  Reg: TRegistry;

const
  AssocPrefix = 'for:';
  SectionPrefix = '\Software\R3R\';

function ReadAssocString(const Name: String): String;
var
  OldKey: HKEY;
  OldPath: String;
begin
  with Reg do
  begin
    OldKey := RootKey;
    OldPath := CurrentPath;
    CloseKey;

    RootKey := HKEY_CLASSES_ROOT;
    OpenKey(Name + '\shell\open\command', true);
    ReadAssocString := ReadString('');
    CloseKey;

    RootKey := OldKey;
    OpenKey(OldPath, true);
  end;
end;

procedure WriteAssocString(const Name, Value: String);
var
  OldKey: HKEY;
  OldPath: String;
begin
  with Reg do
  begin
    OldKey := RootKey;
    OldPath := CurrentPath;
    CloseKey;

    RootKey := HKEY_CLASSES_ROOT;
    OpenKey(Name + '\shell\open\command', true);
    WriteString('', Value);
    CloseKey;

    RootKey := OldKey;
    OpenKey(OldPath, true);
  end;
end;

constructor TRSettings.Create;
begin
  Reg := TRegistry.Create;
  Reg.RootKey := HKEY_CURRENT_USER;

  New(FSettings, Init);
  InitRec;
end;

destructor TRSettings.Destroy;
begin
  WriteRec;

  Dispose(FSettings, Done);
  Reg.Free;
end;

procedure TRSettings.CheckBoolean(const Setting, ASection: String; const Value: Boolean);
var
  ASetting: PRSetting;
  Exists: Boolean;
begin
  New(ASetting);
  with ASetting^ do
  begin
    Reg.OpenKey(SectionPrefix + ASection, true);
    Exists := Reg.ValueExists(Setting);
    
    Name := Setting;
    Section := ASection;
    ValueType := TypeBoolean;

    if Exists then
    begin
      ValueBoolean := Reg.ReadBool(Setting);
    end
    else
    begin
      ValueBoolean := Value;
    end;
  end;
  FSettings^.Add(ASetting);

  Reg.CloseKey;
end;

procedure TRSettings.CheckInteger(const Setting, ASection: String; const Value: integer);
var
  ASetting: PRSetting;
  Exists: Boolean;
begin
  New(ASetting);
  with ASetting^ do
  begin
    Reg.OpenKey(SectionPrefix + ASection, true);
    Exists := Reg.ValueExists(Setting);

    Name := Setting;
    Section := ASection;
    ValueType := TypeInteger;

    if Exists then
    begin
      ValueInteger := Reg.ReadInteger(Setting);
    end
    else
    begin
      ValueInteger := Value;
    end;
  end;
  FSettings^.Add(ASetting);

  Reg.CloseKey;
end;

procedure TRSettings.CheckString(const Setting, ASection, Value: String);
var
  AName: String;
  ASetting: PRSetting;
  Exists: Boolean;
  Index: byte;
begin
  New(ASetting);
  with ASetting^ do
  begin
    Reg.OpenKey(SectionPrefix + ASection, true);
    Exists := Reg.ValueExists(Setting);
    Index := Pos(AssocPrefix, Setting);

    Name := Setting;
    Section := ASection;
    ValueString := Reg.ReadString(Setting);
    ValueType := TypeString;

    if Exists then
    begin
      ValueString := Reg.ReadString(Setting);
    end
    else if Index = 1 then
    begin
      AName := Setting;
      Delete(AName, 1, Length(AssocPrefix));
      ValueString := ReadAssocString(AName);
      Exists := ValueString <> '';
    end;

    if not Exists then
    begin
      ValueString := Value;
    end;
  end;
  FSettings^.Add(ASetting);

  Reg.CloseKey;
end;

procedure TRSettings.ReadRec;
begin
end;

procedure TRSettings.WriteRec;
var
  ASetting: PRSetting;
  Index: byte;
  i: byte;
begin
  for i := 0 to FSettings^.Count - 1 do
  begin
    ASetting := PRSetting(FSettings^.GetNth(i));

    with ASetting^, Reg do
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
          Index := Pos(AssocPrefix, Name);
          if Index <> 1 then
          begin
            if ValueString <> '' then
            begin
              WriteString(Name, ValueString);
            end;
          end
          else
          begin
            Delete(Name, Index, Length(AssocPrefix));
            WriteAssocString(Name, ValueString);
          end;
        end;
      end;

      CloseKey;
    end;

    Dispose(ASetting);
  end;
end;
