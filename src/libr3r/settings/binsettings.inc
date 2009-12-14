function IsValidSetting(Setting: String): Boolean;
begin
  Result := (Setting = 'accept-langs') or
    (Setting = 'accept-types') or (Setting = 'browser') or
    (Setting = 'check-for-updates') or
    (Setting = 'display-feed-title-only') or (Setting = 'editor') or
    (Setting = 'enable-mime-guess') or
    (Setting = 'hide-cached-feed-items') or
    (Setting = 'hide-cached-feeds') or
    (Setting = 'load-subscriptions-on-startup') or
    (Setting = 'mail-client') or (Setting = 'proxy-address') or
    (Setting = 'proxy-port') or (Setting = 'show-messages') or
    (Setting = 'use-custom-accept-langs') or
    (Setting = 'use-custom-accept-types') or (Setting = 'use-proxy');
end;

constructor TRSettings.Create;
begin
  inherited Create;
  New(FSettings, Init);

  FSettingsFile := SettingsDir + 'r3rrc';

  if FileExists(FSettingsFile) then
  begin
    ReadRec;
  end;

  InitRec;
end;

destructor TRSettings.Destroy;
begin
  WriteRec;

  Dispose(FSettings, Done);
  inherited Destroy;
end;

function TRSettings.CheckBoolean(const Setting, ASection: String; const Value: Boolean): Boolean;
var
  ASetting: PRSetting;
begin
  Result := IndexOf(Setting) <> -1;

  if not Result then
  begin
    New(ASetting);
    with ASetting^ do
    begin
      Name := Setting;
      Section := ASection;
      ValueBoolean := Value;
      ValueType := TypeBoolean;
      FSettings^.Add(ASetting);
    end;
  end;
end;

function TRSettings.CheckInteger(const Setting, ASection: String; const Value: Integer): Boolean;
var
  ASetting: PRSetting;
begin
  Result := IndexOf(Setting) <> -1;

  if not Result then
  begin
    New(ASetting);
    with ASetting^ do
    begin
      Name := Setting;
      Section := ASection;
      ValueInteger := Value;
      ValueType := TypeInteger;
      FSettings^.Add(ASetting);
    end;
  end;
end;

function TRSettings.CheckString(const Setting, ASection, Value: String): Boolean;
var
  ASetting: PRSetting;
begin
  Result := IndexOf(Setting) <> -1;

  if not Result then
  begin
    New(ASetting);
    with ASetting^ do
    begin
      Name := Setting;
      Section := ASection;
      ValueString := Value;
      ValueType := TypeString;
      FSettings^.Add(ASetting);
    end;
  end;
end;

procedure TRSettings.ReadRec;
var
  ASetting: PRSetting;
  f: file;
  Last: String;
  NumRead: integer;
begin
  Assign(f, FSettingsFile);
  Reset(f, 1);

  repeat
    New(ASetting);
    BlockRead(f, ASetting^, SizeOf(ASetting^), NumRead);

    if (ASetting^.Name <> Last) and IsValidSetting(ASetting^.Name) then
    begin
      Last := ASetting^.Name;
      FSettings^.Add(ASetting);
    end
    else
    begin
      Dispose(ASetting);
    end;
  until NumRead = 0;

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

  Assign(f, FSettingsFile);
  Rewrite(f, 1);

  if FSettings^.Count > 0 then
  begin
    for i := 0 to FSettings^.Count - 1 do
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
