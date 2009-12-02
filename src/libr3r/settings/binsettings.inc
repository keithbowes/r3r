function IsValidSetting(Setting: String): Boolean;
begin
  Result := (Setting = 'accept-langs') or (Setting = 'accept-types') or
    (Setting = 'browser') or (Setting = 'display-feed-title-only') or
    (Setting = 'editor') or (Setting = 'enable-mime-guess') or
    (Setting = 'hide-cached-feed-items') or (Setting = 'hide-cached-feeds') or
    (Setting = 'mail-client') or (Setting = 'proxy-address') or
    (Setting = 'proxy-port') or (Setting = 'show-messages') or
    (Setting = 'subscriptions') or (Setting = 'use-custom-accept-langs') or
    (Setting = 'use-custom-accept-types') or (Setting = 'use-proxy');
end;

constructor TRSettings.Create;
begin
  inherited Create;

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
  inherited Destroy;
end;

function TRSettings.CheckBoolean(const Setting, ASection: String; const Value: Boolean): Boolean;
begin
  Result := IndexOf(Setting) <> -1;

  if not Result then
  begin
    with FSettings[FRef] do
    begin
      Name := Setting;
      Section := ASection;
      ValueBoolean := Value;
      ValueType := TypeBoolean;
      Inc(FRef);
    end;
  end;
end;

function TRSettings.CheckInteger(const Setting, ASection: String; const Value: Integer): Boolean;
begin
  Result := IndexOf(Setting) <> -1;

  if not Result then
  begin
    with FSettings[FRef] do
    begin
      Name := Setting;
      Section := ASection;
      ValueInteger := Value;
      ValueType := TypeInteger;
      Inc(FRef);
    end;
  end;
end;

function TRSettings.CheckString(const Setting, ASection, Value: String): Boolean;
begin
  Result := IndexOf(Setting) <> -1;

  if not Result then
  begin
    with FSettings[FRef] do
    begin
      Name := Setting;
      Section := ASection;
      ValueString := Value;
      ValueType := TypeString;
      Inc(FRef);
    end;
  end;
end;

{ Clean up the corrupted elements }
function TRSettings.CleanRec: Boolean;
var
  i: byte;
  TmpSet: TRSettingsRec;
  ValueType: byte;
begin
  FRef := 0;
  TmpSet := FSettings;
  Result := false;

  for i := 1 to SettingsCount do
  begin
    ValueType := TmpSet[i].ValueType;

    if not IsValidSetting(TmpSet[i].Name) then
    begin
      ValueType := TypeNone;
    end;

    if ValueType <> TypeNone then
    begin
      Inc(FRef);
      FSettings[FRef] := TmpSet[i];
    end
    else
    begin
      Result := true;
      if FRef > 0 then
      begin
        Dec(FRef);
      end;
    end;
  end;

  if FRef = 0 then
  begin
    FRef := 1;
  end
  else if Result then
  begin
    Dec(FRef);
  end;
end;

procedure TRSettings.ReadRec;
var
  f: file of TRSettingsRec;
  NumRead: integer;
begin
  Assign(f, FSettingsFile);
  Reset(f, 1);

  repeat
    BlockRead(f, FSettings, SizeOf(FSettings), NumRead);
  until NumRead = 0;

  Close(f);
end;

procedure TRSettings.WriteRec;
var
  f: file of TRSettingsRec;
  LastNum, NumWritten: integer;
begin
  Assign(f, FSettingsFile);
  Rewrite(f, 1);

  repeat
    LastNum := NumWritten;
    BlockWrite(f, FSettings, SizeOf(FSettings), NumWritten);
  until (NumWritten = 0) or (NumWritten = LastNum);

  Close(f);
end;
