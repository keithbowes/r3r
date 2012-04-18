var
  Reg: TRegistry;

const
  AssocPrefix = 'for:';
  SectionPrefix = '\Software\' + AppName + '\';

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

function TRSettings.GetBoolean(const Setting: String): Boolean;
var
	Index: TRSetIndex;
  Sett: TRSetting;
begin
	Index := IndexOf(Setting);
  if Index <> -1 then
  begin
    Sett := PRSetting(FSettings^.GetNth(Index))^;
    Reg.OpenKey(SectionPrefix + Sett.Section, true);
    GetBoolean := Reg.ReadBool(Sett.Name);
    Reg.CloseKey;
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
    Reg.OpenKey(SectionPrefix + Sett.Section, true);
    GetInteger := Reg.ReadInteger(Sett.Name);
    Reg.CloseKey;
  end;
end;

function TRSettings.GetString(const Setting: String): String;
var
	Index: TRSetIndex;
  Name: String;
  Sett: TRSetting;
begin
	Index := IndexOf(Setting);
  if Index <> -1 then
  begin
    Sett := PRSetting(FSettings^.GetNth(Index))^;
    if Pos(AssocPrefix, Sett.Name) <> 1 then
    begin
      Reg.OpenKey(SectionPrefix + Sett.Section, true);
      GetString := Reg.ReadString(Sett.Name);
      Reg.CloseKey;
    end
    else
    begin
      Name := Sett.Name;
      Delete(Name, 1, Length(AssocPrefix));
      GetString := ReadAssocString(Name);
    end;
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
      Reg.OpenKey(SectionPrefix + Section, true);
      Reg.WriteBool(Name, Value);
      Reg.CloseKey;
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
      Reg.OpenKey(SectionPrefix + Section, true);
      Reg.WriteInteger(Name, Value);
      Reg.CloseKey;
			ValueInteger := Value;
    end;
  end;
end;

procedure TRSettings.SetString(const Setting: String; const Value: String);
var
  AName: String;
	Index: TRSetIndex;
begin
	Index := IndexOf(Setting);
  if Index <> -1 then
  begin
    with PRSetting(FSettings^.GetNth(Index))^ do
    begin
      if Pos(AssocPrefix, Name) <> 1 then
      begin
        Reg.OpenKey(SectionPrefix + Section, true);
        Reg.WriteString(Name, Value);
        Reg.CloseKey;
      end
      else
      begin
        AName := Name;
        Delete(AName, 1, Length(AssocPrefix));
        WriteAssocString(AName, Value)
      end;

			ValueString := Value;
    end;
  end;
end;

procedure TRSettings.CheckBoolean(const Setting, ASection: String; const Value: Boolean; const ADescription: String);
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
    Description := ADescription;

    if Exists then
    begin
      ValueBoolean := Reg.ReadBool(Setting);
    end
    else
    begin
      Reg.WriteBool(Setting, Value);
      ValueBoolean := Value;
    end;
  end;
  FSettings^.Add(ASetting);

  Reg.CloseKey;
end;

procedure TRSettings.CheckInteger(const Setting, ASection: String; const Value: integer; const ADescription: String);
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
    Description := ADescription;

    if Exists then
    begin
      ValueInteger := Reg.ReadInteger(Setting);
    end
    else
    begin
      Reg.WriteInteger(Setting, Value);
      ValueInteger := Value;
    end;
  end;
  FSettings^.Add(ASetting);

  Reg.CloseKey;
end;

procedure TRSettings.CheckString(const Setting, ASection, Value, ADescription: String);
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
    Description := ADescription;
    ValueType := TypeString;

    if Exists then
    begin
      ValueString := Reg.ReadString(Setting);
    end
    else if Index = 1 then
    begin
			Reg.CloseKey;

      AName := Setting;
      Delete(AName, 1, Length(AssocPrefix));
      ValueString := ReadAssocString(AName);

			Reg.OpenKey(SectionPrefix + ASection, true);
      Exists := true;
    end;

    if not Exists then
    begin
      Reg.WriteString(Setting, Value);
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
    Dispose(ASetting);
  end;
end;

{
  vi:filetype=pascal
}
