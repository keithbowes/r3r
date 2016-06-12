var
  IniFile: ini_fd_t;

procedure FindKey(Section, Name: String);
begin
  ini_locateHeading(IniFile, StrToPChar(Section));
  ini_locateKey(IniFile, StrToPChar(Name));
end;

constructor TRSettings.Create;
begin
  IniFile := ini_open(StrToPChar(GetSettingsDir + 'r3r.ini'), 'w', ';');
  New(FSettings, Init);
  InitRec;
end;

destructor TRSettings.Destroy;
begin
  WriteRec;
  ini_close(IniFile);
  Dispose(FSettings, Done);
end;

function TRSettings.GetBoolean(const Setting: String): Boolean;
var
  Index: TRSetIndex;
  Res: integer;
  Sett: TRSetting;
begin
  Index := IndexOf(Setting);
  if Index <> -1 then
  begin
    Sett := PRSetting(FSettings^.GetNth(Index))^;
    FindKey(Sett.Section, Sett.Name);
    if ini_readInt(IniFile, Res) < 0 then
    begin
      Res := Ord(Sett.ValueBoolean);
    end;

    GetBoolean := Res <> 0;
  end;
end;

function TRSettings.GetInteger(const Setting: String): integer;
var
  Index: TRSetIndex;
  Res: integer;
  Sett: TRSetting;
begin
  Index := IndexOf(Setting);
  if Index <> -1 then
  begin
    Sett := PRSetting(FSettings^.GetNth(Index))^;
    FindKey(Sett.Section, Sett.Name);
    if ini_readInt(IniFile, Res) < 0 then
    begin
      Res := Sett.ValueInteger;
    end;

    GetInteger := Res;
  end;
end;

function TRSettings.GetString(const Setting: String): String;
var
  Index: TRSetIndex;
  Len: integer;
  Res: char_p;
  Sett: TRSetting;
  Sres: String;
begin
  Index := IndexOf(Setting);
  if Index <> -1 then
  begin
    Sett := PRSetting(FSettings^.GetNth(Index))^;
    FindKey(Sett.Section, Sett.Name);
    Len := ini_dataLength(IniFile) + 1;
    GetMem(Res, Len);
    if ini_readString(IniFile, Res, Len) < 0 then
    begin
      Res := StrToPChar(Sett.ValueString);
    end;

    WriteStr(Sres, Res);
    GetString := sRes;
    if Len > 0 then
    begin
      FreeMem(Res);
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
      FindKey(Section, Name);
      ini_writeInt(IniFile, Ord(Value));
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
      FindKey(Section, Name);
      ini_writeInt(IniFile, Value);
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
      FindKey(Section, Name);
      ini_writeString(IniFile, StrToPChar(Value));
      ValueString := Value;
    end;
  end;
end;

procedure TRSettings.CheckBoolean(const Setting, ASection: String; const Value: Boolean; const ADescription: String);
var
  ASetting: PRSetting;
  Res: integer;
begin
  New(ASetting);
  with ASetting^ do
  begin
    FindKey(ASection, Setting);
    if ini_readInt(IniFile, Res) < 0 then
    begin
      Res := Ord(Value);
    end;
    ini_writeInt(IniFile, Res);

    Name := Setting;
    Section := ASection;
    ValueBoolean := Res <> 0;
    ValueType := TypeBoolean;
  end;

  ASetting^.Description := ADescription;
  FSettings^.Add(ASetting);
end;

procedure TRSettings.CheckInteger(const Setting, ASection: String; const Value: integer; const ADescription: String);
var
  ASetting: PRSetting;
  Res: integer;
begin
  New(ASetting);
  with ASetting^ do
  begin
    FindKey(ASection, Setting);
    if ini_readInt(IniFile, Res) < 0 then
    begin
      Res := Value;
    end;
    ini_writeInt(IniFile, Res);

    Name := Setting;
    Section := ASection;
    ValueInteger := Res;
    ValueType := TypeInteger;
  end;

  ASetting^.Description := ADescription;
  FSettings^.Add(ASetting);
end;

procedure TRSettings.CheckString(const Setting, ASection, Value, ADescription: String);
var
  ASetting: PRSetting;
  Len: integer;
  Res: char_p;
begin
  New(ASetting);
  with ASetting^ do
  begin
    FindKey(ASection, Setting);
    Len := ini_dataLength(IniFile) + 1;
    GetMem(Res, Len);
    if ini_readString(IniFile, Res, Len) >= 0 then
    begin
      WriteStr(ValueString, Res);
    end
    else
    begin
      ValueString := Value;
      ini_writeString(IniFile, StrToPChar(Value));
    end;

    Name := Setting;
    Section := ASection;
    ValueType := TypeString;
    FreeMem(Res);
  end;

  ASetting^.Description := ADescription;
  FSettings^.Add(ASetting);
end;

procedure TRSettings.WriteRec;
var
  ASetting: PRSetting;
  i: byte;
  s: String;
begin
  for i := 0 to FSettings^.Count - 1 do
  begin
    ASetting := PRSetting(FSettings^.GetNth(i));
    with ASetting^ do
    begin
      FindKey(Section, Name);  
      case ValueType of
        TypeBoolean:
        begin
          WriteStr(s, Ord(ValueBoolean), ' ', CommentSeparator, Description);
        end;
        TypeInteger:
        begin
          WriteStr(s, ValueInteger, ' ', CommentSeparator, Description);
        end;
        TypeString:
        begin
          WriteStr(s, ValueString, ' ', CommentSeparator, Description);
        end;
      end;
      ini_writeString(IniFile, StrToPChar(s));
    end;
    Dispose(ASetting);
  end;
end;

{
  vi:filetype=pascal
}
