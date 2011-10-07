var
  IniFile: ini_fd_t;

procedure FindKey(Section, Name: String);
begin
	ini_locateHeading(IniFile, StrToPChar(Section));
	ini_locateKey(IniFile, StrToPChar(Name));
end;

constructor TRSettings.Create;
begin
  IniFile := ini_open(StrToPChar(SettingsDir + 'r3r.ini'), 'w', ';');
	New(FSettings, Init);
  InitRec;
end;

destructor TRSettings.Destroy;
begin
	ini_close(IniFile);
	WriteRec;
	Dispose(FSettings, Done);
end;

function TRSettings.GetBoolean(const Index: TRSetIndex): Boolean;
var
	Res: bool;
  Sett: TRSetting;
begin
  if Index <> -1 then
  begin
    Sett := PRSetting(FSettings^.GetNth(Index))^;
		FindKey(Sett.Section, Sett.Name);
		if ini_readBool(IniFile, Res) < 0 then
		begin
			Res := Sett.ValueBoolean;
		end;

    GetBoolean := Res;
  end;
end;

function TRSettings.GetInteger(const Index: TRSetIndex): integer;
var
	Res: integer;
  Sett: TRSetting;
begin
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

function TRSettings.GetString(const Index: TRSetIndex): String;
var
	Len: integer;
	Res: char_p;
  Sett: TRSetting;
begin
  if Index <> -1 then
  begin
    Sett := PRSetting(FSettings^.GetNth(Index))^;
		FindKey(Sett.Section, Sett.Name);
		Len := ini_dataLength(IniFile);
		GetMem(Res, Len + 1);
		if ini_readString(IniFile, Res, Len + 1) < 0 then
		begin
			Res := StrToPChar(Sett.ValueString);
		end;

		GetString := StrPas(Res);
		if Len > 0 then
		begin
			FreeMem(Res);
		end;
  end;
end;

procedure TRSettings.SetBoolean(const Index: TRSetIndex; const Setting: Boolean);
begin
  if Index <> -1 then
  begin
    with PRSetting(FSettings^.GetNth(Index))^ do
    begin
			FindKey(Section, Name);
      ini_writeBool(IniFile, Setting);
			ValueBoolean := Setting;
    end;
  end;
end;

procedure TRSettings.SetInteger(const Index: TRSetIndex; const Setting: integer);
begin
  if Index <> -1 then
  begin
    with PRSetting(FSettings^.GetNth(Index))^ do
    begin
			FindKey(Section, Name);
      ini_writeInt(IniFile, Setting);
			ValueInteger := Setting;
    end;
  end;
end;

procedure TRSettings.SetString(const Index: TRSetIndex; const Setting: String);
begin
  if Index <> -1 then
  begin
    with PRSetting(FSettings^.GetNth(Index))^ do
    begin
			FindKey(Section, Name);
      ini_writeString(IniFile, StrToPChar(Setting));
			ValueString := Setting;
    end;
  end;
end;

procedure TRSettings.CheckBoolean(const Setting, ASection: String; const Value: Boolean; const ADescription: String);
var
  ASetting: PRSetting;
	Res: bool;
begin
  New(ASetting);
  with ASetting^ do
  begin
		FindKey(ASection, Setting);
		if ini_readBool(IniFile, Res) < 0 then
		begin
			Res := Value;
		end;
		ini_writeBool(IniFile, Res);

		Name := Setting;
		Section := ASection;
    ValueBoolean := Res;
    ValueType := TypeBoolean;
  end;

  ASetting.Description := ADescription;
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

  ASetting.Description := ADescription;
  FSettings^.Add(ASetting);
end;

procedure TRSettings.CheckString(const Setting, ASection, Value, ADescription: String);
var
  ASetting: PRSetting;
	Res: char_p;
begin
  New(ASetting);
  with ASetting^ do
  begin
		FindKey(ASection, Setting);
		GetMem(Res, ini_dataLength(IniFile) + 1);
		if ini_readString(IniFile, Res, ini_dataLength(IniFile) + 1) >= 0 then
		begin
			ValueString := StrPas(Res);
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

  ASetting.Description := ADescription;
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
    Dispose(ASetting);
  end;
end;

{
  vi:filetype=pascal
}
