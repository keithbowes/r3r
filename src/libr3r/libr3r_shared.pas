library LibR3R_Shared;

uses
  Info, LibR3R, RSettings, RStrings, SysUtils;

const
  SubscriptionAdd = 1;
  SubscriptionDelete = 2;
  SubscriptionGet = 3;

type
  TMessageProc = procedure(IsError: byte; MessageName, Extra: PChar); cdecl;
  TParsedProc = procedure(Item: Pointer); cdecl;
  TUpdateProc = procedure; cdecl;

  TLibR3R_Shared = class(TLibR3R)
  private
    FMessageProc: TMessageProc;
    FParsedProc: TParsedProc;
    FUpdateProc: TUpdateProc;
  public
    procedure DisplayItem(const Item: TFeedItem); override;
    procedure HandleMessage(IsError: Boolean; MessageName, Extra: String); override;
    procedure NotifyUpdate; override;

    property MessageProc: TMessageProc write FMessageProc;
    property ParsedProc: TParsedProc write FParsedProc;
    property UpdateProc: TUpdateProc write FUpdateProc;
  end;

{ Implemetation of the helper class }
procedure TLibR3R_Shared.DisplayItem(const Item: TFeedItem);
begin
  if Assigned(FParsedProc) then
  begin
    FParsedProc(Item);
  end;
end;

procedure TLibR3R_Shared.HandleMessage(IsError: Boolean; MessageName, Extra: String);
begin
  if Assigned(FMessageProc) then
  begin
    FMessageProc(Ord(IsError), StrToPChar(MessageName), StrToPChar(Extra));
  end;
end;

procedure TLibR3R_Shared.NotifyUpdate;
begin
  if Assigned(FUpdateProc) then
  begin
    FUpdateProc;
  end;
end;

{ Public exported functions }
function libr3r_create: Pointer; cdecl;
begin
  libr3r_create := TLibR3R_Shared.Create;
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

procedure libr3r_on_update(Lib: Pointer; Proc: TUpdateProc); cdecl;
begin
  TLibR3R_Shared(Lib).UpdateProc := Proc;
end;

function libr3r_get_item_field(Item: Pointer; FieldName: PChar): Pointer; cdecl;
var
  i: cardinal;
begin
  with TFeedItem(Item) do
  begin
    if FieldName = 'title' then
    begin
      libr3r_get_item_field := StrToPChar(Title);
    end
    else if FieldName = 'title-text' then
    begin
      libr3r_get_item_field := StrToPChar(TitleText);
    end
    else if FieldName = 'links' then
    begin
      for i := 0 to LinksCount do
      begin
        libr3r_get_item_field := StrCat(Links^.GetNth(i), #147);
      end;
    end
    else if FieldName = 'link-count' then
    begin
      libr3r_get_item_field := Pointer(PtrInt(LinksCount));
    end
    else if FieldName = 'main-link' then
    begin
      libr3r_get_item_field := StrToPChar(GetMainLink)
    end
    else if FieldName = 'podcast' then
    begin
      libr3r_get_item_field := StrToPChar(GetPodcast);
    end
    else if FieldName = 'description' then
    begin
      libr3r_get_item_field := StrToPChar(Description)
    end
    else if FieldName = 'description-text' then
    begin
      libr3r_get_item_field := StrToPChar(DescriptionText)
    end
    else if FieldName = 'subject' then
    begin
      libr3r_get_item_field := StrToPChar(Subject);
    end
    else if FieldName = 'created' then
    begin
      libr3r_get_item_field := StrToPChar(Created);
    end
    else if FieldName = 'contact-name' then
    begin
      libr3r_get_item_field := StrToPChar(Contact^.Name);
    end
    else if FieldName = 'contact-email' then
    begin
      libr3r_get_item_field := StrToPChar(Contact^.Email);
    end
    else if FieldName = 'contact-uri' then
    begin
      libr3r_get_item_field := StrToPChar(Contact^.URI);
    end
    else if FieldName = 'generator' then
    begin
      libr3r_get_item_field := StrToPChar(Generator);
    end
    else if FieldName = 'last-modified' then
    begin
      libr3r_get_item_field := StrToPChar(LastModified);
    end
    else if FieldName = 'language' then
    begin
      libr3r_get_item_field := StrToPChar(Language);
    end
    else if FieldName = 'id' then
    begin
      libr3r_get_item_field := StrToPChar(Id);
    end
    else if FieldName = 'copyright' then
    begin
      libr3r_get_item_field := StrToPChar(Copyright);
    end
    else if FieldName = 'uri' then
    begin
      libr3r_get_item_field := StrToPChar(Uri);
    end
    else if FieldName = 'myself' then
    begin
      libr3r_get_item_field := StrToPChar(Myself);
    end
    else
    begin
      libr3r_get_item_field := nil;
    end;
  end;
end;

function libr3r_get_user_agent: PChar; cdecl;
begin
  libr3r_get_user_agent := StrToPChar(UserAgent);
end;

procedure libr3r_register_setting(setting_name, setting_section: PChar; setting_value: Pointer; setting_type: word; descr: PChar); cdecl;
begin
  case setting_type of
    TypeBoolean:
    begin
      Settings.RegisterBoolean(StrPas(setting_name), StrPas(setting_section), PtrUInt(setting_value) <> 0, StrPas(descr));
    end;
    TypeInteger:
    begin
      Settings.RegisterInteger(StrPas(setting_name), StrPas(setting_section), integer(setting_value), StrPas(descr));
    end;
    TypeString:
    begin
      Settings.RegisterString(StrPas(setting_name), StrPas(setting_section), StrPas(setting_value), StrPas(descr));
    end;
  end;
end;

procedure libr3r_access_settings(var Index: integer; var setting_name: PChar; var SettingValue: Pointer; var SettingType: byte; var Count: integer; const SettingsMode: byte); cdecl;
var
  SettingName: ShortString;
begin
  RemoveDuplicatePChars := false;

  SettingName := StrPas(setting_name);
  Settings.Access(Index, SettingName, SettingValue, SettingType, Count, SettingsMode);
  setting_name := StrToPChar(SettingName);
end;

procedure libr3r_access_subscriptions(Index, Mode: byte; var Subscription: PChar; var Count: word); cdecl;
begin
  RemoveDuplicatePChars := false;
  Count := Subscriptions^.Count;

  if Mode = SubscriptionAdd then
  begin
    Subscriptions^.Add(StrPas(Subscription));
  end
  else if Mode = SubscriptionDelete then
  begin
    if (Index = 0) and (Subscription <> nil) then
    begin
      Subscriptions^.DeleteString(StrPas(Subscription));
    end
    else
    begin
      Subscriptions^.DeleteIndex(Index);
    end
  end
  else if Mode = SubscriptionGet then
  begin
    Subscription := StrToPChar(Subscriptions^.GetNth(Index));
  end;
end;

exports
  libr3r_create, libr3r_free, libr3r_retrieve_feed,
  libr3r_on_item_parsed, libr3r_on_message_received, libr3r_on_update,
  libr3r_get_item_field,
  libr3r_get_user_agent,
  libr3r_register_setting, libr3r_access_settings,
  libr3r_access_subscriptions;

end.
