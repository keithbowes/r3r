function IsValidSetting(Setting: String): Boolean;
begin
  IsValidSetting := (Setting = 'accept-langs') or
    (Setting = 'accept-types') or
    (Setting = 'check-for-updates') or
		(Setting = 'display-encoding') or
    (Setting = 'display-feed-title-only') or
    (Setting = 'enable-mime-guess') or
    (Setting = 'for:.ogg') or (Setting = 'for:http') or
    (Setting = 'for:mailto') or
    (Setting = 'hide-cached-feed-items') or
    (Setting = 'hide-cached-feeds') or
		(Setting = 'installed-prefix') or
    (Setting = 'load-subscriptions-on-startup') or
    (Setting = 'proxy-address') or (Setting = 'proxy-port') or
    (Setting = 'show-messages') or 
		(Setting = 'use-filters') or
    (Setting = 'use-custom-accept-langs') or
    (Setting = 'use-custom-accept-types') or
    (Setting = 'use-proxy') or
		(Setting = 'warn-missing-data');
end;

var
	SettingsFile: String;

constructor TRSettings.Create;
begin
{$IFNDEF __GPC__}
  inherited Create;
{$ENDIF}
  SettingsFile := SettingsDir + 'r3r.dat';
  New(FSettings, Init);

  if FileExists(SettingsFile) then
  begin
    ReadRec;
  end;

  InitRec;
end;

destructor TRSettings.Destroy;
begin
  WriteRec;

  Dispose(FSettings, Done);

{$IFNDEF __GPC__}
  inherited Destroy;
{$ENDIF}
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
var
  Setting: TRSetIndex;
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
  Exists: Boolean;
begin
  Exists := IndexOf(Setting) <> -1;
  New(ASetting);

  with ASetting^ do
  begin
		if not Exists then
		begin
      Name := Setting;
      Section := ASection;
      ValueBoolean := Value;
			ValueType := TypeBoolean;
			FSettings^.Add(ASetting);
		end;

		ASetting^.Description := ADescription;
  end;

  if Exists then
	begin
    Dispose(ASetting);
  end;
end;

procedure TRSettings.CheckInteger(const Setting, ASection: String; const Value: Integer; const ADescription: String);
var
  ASetting: PRSetting;
  Exists: Boolean;
begin
  Exists := IndexOf(Setting) <> -1;
  New(ASetting);

  with ASetting^ do
  begin
		if not Exists then
		begin
      Name := Setting;
      Section := ASection;
      ValueInteger := Value;
			ValueType := TypeInteger;
			FSettings^.Add(ASetting);
		end;

		ASetting^.Description := ADescription;
  end;

  if Exists then
	begin
    Dispose(ASetting);
  end;
end;

procedure TRSettings.CheckString(const Setting, ASection, Value, ADescription: String);
var
  ASetting: PRSetting;
  Exists: Boolean;
begin
  Exists := IndexOf(Setting) <> -1;
  New(ASetting);

  with ASetting^ do
  begin
		if not Exists then
		begin
      Name := Setting;
      Section := ASection;
      ValueString := Value;
			ValueType := TypeString;
			FSettings^.Add(ASetting);
		end;

		ASetting^.Description := ADescription;
  end;

  if Exists then
	begin
    Dispose(ASetting);
  end;
end;

procedure TRSettings.ReadRec;
var
  ASetting: PRSetting;
  f: file;
  Last: String;
  NumRead: integer;
begin
  Assign(f, SettingsFile);
  Reset(f, 1);

  repeat
    New(ASetting);
    BlockRead(f, ASetting^, SizeOf(ASetting^), NumRead);

    if (ASetting^.Name <> Last) and IsValidSetting(ASetting^.Name) then
    begin
      FSettings^.Add(ASetting);
    end
		else
		begin
			Dispose(ASetting)
		end
  until NumRead = 0;

	Last := ASetting^.Name;
  Close(f);
end;

procedure TRSettings.WriteRec;
var
  ASetting: PRSetting;
  f: file;
  i: word;
  LastNum, NumWritten: integer;
begin
  i := 0;

  Assign(f, SettingsFile);
  Rewrite(f, 1);

  if FSettings^.Count > 0 then
  begin
    for i := 1 to FSettings^.Count do
    begin
      ASetting := FSettings^.GetNth(i);
      repeat
        LastNum := NumWritten;
        BlockWrite(f, ASetting^, SizeOf(ASetting^), NumWritten);
      until (NumWritten = LastNum) or (NumWritten = 0);

      Dispose(ASetting);
    end;
  end;

  Close(f);
end;

{
  vi:filetype=pascal
}
