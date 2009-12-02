library LibR3R_Shared;

uses
  Info, LibR3R, RSettings, RStrings;

type
  TMessageProc = procedure(IsError: byte; MessageName, Extra: PChar); cdecl;
  TParsedProc = procedure(Item: Pointer); cdecl;

  TLibR3R_Shared = class(TLibR3R)
  private
    FMessageProc: TMessageProc;
    FParsedProc: TParsedProc;
  public
    procedure DisplayItem(const Item: TFeedItem); override;
    procedure HandleMessage(Sender: TObject; IsError: Boolean; MessageName, Extra: String); override;

    property MessageProc: TMessageProc write FMessageProc;
    property ParsedProc: TParsedProc write FParsedProc;
  end;

{ Implemetation of the helper class }
procedure TLibR3R_Shared.DisplayItem(const Item: TFeedItem);
begin
  if Assigned(FParsedProc) then
  begin
    FParsedProc(Item);
  end;
end;

procedure TLibR3R_Shared.HandleMessage(Sender: TObject; IsError: Boolean; MessageName, Extra: String);
begin
  if Assigned(FMessageProc) then
  begin
    FMessageProc(Ord(IsError), StrToPChar(MessageName), StrToPChar(Extra));
  end;
end;

{ Public exported functions }
function libr3r_create: Pointer; cdecl;
begin
  Result := TLibR3R_Shared.Create;
end;

procedure libr3r_free(Lib: Pointer); cdecl;
begin
  TLibR3R_Shared(Lib).Free;
end;

procedure libr3r_retrieve_feed(Lib: Pointer; Resource: PChar); cdecl;
begin
  TLibR3R_Shared(Lib).RetrieveFeed(StrPas(Resource));
end;

procedure libr3r_on_item_parsed(Lib: Pointer; Proc: TParsedProc); cdecl;
begin
  TLibR3R_Shared(Lib).ParsedProc := Proc;
end;

procedure libr3r_on_message_received(Lib: Pointer; Proc: TMessageProc); cdecl;
begin
  TLibR3R_Shared(Lib).MessageProc := Proc;
end;

function libr3r_get_item_field(Item: Pointer; FieldName: PChar): Pointer; cdecl;
var
  i: cardinal;
begin
  with TFeedItem(Item) do
  begin
    if FieldName = 'title' then
    begin
      Result := StrToPChar(Title);
    end
    else if FieldName = 'links' then
    begin
      for i := 0 to LinksCount do
      begin
        String(Result) := String(Links^.GetNth(i)) + #147;
      end;
    end
    else if FieldName = 'link-count' then
    begin
      Result := Pointer(PtrInt(LinksCount));
    end
    else if FieldName = 'main-link' then
    begin
      Result := StrToPChar(MainLink)
    end
    else if FieldName = 'description' then
    begin
      Result := StrToPChar(Description)
    end
    else if FieldName = 'subject' then
    begin
      Result := StrToPChar(Subject);
    end
    else if FieldName = 'created' then
    begin
      Result := StrToPChar(Created);
    end
    else if FieldName = 'contact-name' then
    begin
      Result := StrToPChar(Contact^.Toee);
    end
    else if FieldName = 'contact-email' then
    begin
      Result := StrToPChar(Contact^.Address);
    end
    else if FieldName = 'generator' then
    begin
      Result := StrToPChar(Generator);
    end
    else if FieldName = 'last-modified' then
    begin
      Result := StrToPChar(LastModified);
    end
    else if FieldName = 'language' then
    begin
      Result := StrToPChar(Language);
    end
    else if FieldName = 'id' then
    begin
      Result := StrToPChar(Id);
    end
    else if FieldName = 'copyright' then
    begin
      Result := StrToPChar(Copyright);
    end
    else if FieldName = 'uri' then
    begin
      Result := StrToPChar(Uri);
    end
    else
    begin
      Result := nil;
    end;
  end;
end;

function libr3r_get_user_agent: PChar; cdecl;
begin
  Result := StrToPChar(UserAgent);
end;

procedure libr3r_access_settings(var Index: byte; var setting_name: PChar; var SettingValue: Pointer; var SettingType, Count: byte; const SettingsMode: byte); cdecl;
var
  SettingName: ShortString;
begin
  SettingName := StrPas(setting_name);
  Settings.Access(TRSetIndex(Index), SettingName, SettingValue, SettingType, TRSetIndex(Count), SettingsMode);
  setting_name := StrToPChar(SettingName);
end;

exports
  libr3r_create, libr3r_free,
  libr3r_retrieve_feed, libr3r_on_item_parsed, libr3r_on_message_received,
  libr3r_get_item_field,
  libr3r_get_user_agent,
  libr3r_access_settings;

end.
