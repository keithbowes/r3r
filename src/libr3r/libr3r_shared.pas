library LibR3R_Shared;

uses
  Info, LibR3R, RSettings, RSettings_Routines, RStrings, SysUtils;

const
  SubscriptionAdd = 1;
  SubscriptionDelete = 2;
  SubscriptionGet = 3;

type
  TMessageProc = procedure(IsError: byte; MessageName, Extra: PChar); cdecl;
  TParsedProc = procedure(Item: Pointer; Data: Pointer); cdecl;

  TLibR3R_Shared = class(TLibR3R)
  private
    FMessageProc: TMessageProc;
    FParsedProc: TParsedProc;
  public
    constructor Create;
    destructor Destroy; override;
    procedure HandleMessage(IsError: Boolean; MessageName, Extra: String); override;

    property MessageProc: TMessageProc write FMessageProc;
    property ParsedProc: TParsedProc write FParsedProc;
  end;

var
  CurProc: TParsedProc;
  Lib: TLibR3R_Shared;

procedure ItemReceived(const Item: TFeedItem; const Data: Pointer);
begin
  CurProc(Item, Data);
end;

{ Implemetation of the helper class }
constructor TLibR3R_Shared.Create;
begin
  inherited Create;
  RemoveDuplicatePChars := false;
end;

destructor TLibR3R_Shared.Destroy;
begin
  inherited Destroy;
end;

procedure TLibR3R_Shared.HandleMessage(IsError: Boolean; MessageName, Extra: String);
begin
  if Assigned(FMessageProc) then
  begin
    FMessageProc(Ord(IsError), StrToPCharAlloc(MessageName), StrToPCharAlloc(Extra));
  end;
end;

{ Public exported functions }
procedure libr3r_queue_uri(Resource: PChar); cdecl;
var
  Res: String;
begin
  WriteStr(Res, Resource);
  Lib.QueueURI(Res);
end;

procedure libr3r_unqueue_uri; cdecl;
begin
  Lib.UnqueueURI;
end;

function libr3r_retrieve_chunk: integer; cdecl;
begin
  Result := Ord(Lib.RetrieveChunk);
end;

procedure libr3r_retrieve_feed(Resource: PChar); cdecl;
var
  Res: String;
begin
  WriteStr(Res, Resource);
  Lib.RetrieveFeed(Res);
end;

procedure libr3r_on_item_parsed(Proc: TParsedProc; Data: Pointer); cdecl;
begin
  Lib.RegisterItemCallback(ItemReceived, Data);
  CurProc := Proc;
end;

procedure libr3r_on_message_received(Proc: TMessageProc); cdecl;
begin
  Lib.MessageProc := Proc;
end;

function libr3r_get_item_field(Item: Pointer; FieldName: PChar): Pointer; cdecl;
begin
  with TFeedItem(Item) do
  begin
    if FieldName = 'title' then
    begin
      libr3r_get_item_field := StrToPCharAlloc(Title);
    end
    else if FieldName = 'title-text' then
    begin
      libr3r_get_item_field := StrToPCharAlloc(TitleText);
    end
    else if FieldName = 'link' then
    begin
      libr3r_get_item_field := StrToPCharAlloc(Link);
    end
    else if FieldName = 'enclosure-url' then
    begin
      libr3r_get_item_field := StrToPCharAlloc(Enclosure.URL);
    end
    else if FieldName = 'enclosure-type' then
    begin
      libr3r_get_item_field := StrToPCharAlloc(Enclosure.MimeType);
    end
    else if FieldName = 'description' then
    begin
      libr3r_get_item_field := StrToPCharAlloc(Description)
    end
    else if FieldName = 'description-text' then
    begin
      libr3r_get_item_field := StrToPCharAlloc(DescriptionText)
    end
    else if FieldName = 'subject' then
    begin
      libr3r_get_item_field := StrToPCharAlloc(Subject);
    end
    else if FieldName = 'created' then
    begin
      libr3r_get_item_field := StrToPCharAlloc(Created);
    end
    else if FieldName = 'contact-name' then
    begin
      libr3r_get_item_field := StrToPCharAlloc(Contact.Name);
    end
    else if FieldName = 'contact-email' then
    begin
      libr3r_get_item_field := StrToPCharAlloc(Contact.Email);
    end
    else if FieldName = 'contact-uri' then
    begin
      libr3r_get_item_field := StrToPCharAlloc(Contact.URI);
    end
    else if FieldName = 'generator' then
    begin
      libr3r_get_item_field := StrToPCharAlloc(Generator);
    end
    else if FieldName = 'last-modified' then
    begin
      libr3r_get_item_field := StrToPCharAlloc(LastModified);
    end
    else if FieldName = 'language' then
    begin
      libr3r_get_item_field := StrToPCharAlloc(Language);
    end
    else if FieldName = 'id' then
    begin
      libr3r_get_item_field := StrToPCharAlloc(Id);
    end
    else if FieldName = 'copyright' then
    begin
      libr3r_get_item_field := StrToPCharAlloc(Copyright);
    end
    else if FieldName = 'uri' then
    begin
      libr3r_get_item_field := StrToPCharAlloc(Uri);
    end
    else if FieldName = 'self' then
    begin
      libr3r_get_item_field := StrToPCharAlloc(Myself);
    end
    else if FieldName = 'finished' then
    begin
      libr3r_get_item_field := Pointer(PtrInt(Finished));
    end
    else
    begin
      libr3r_get_item_field := nil;
    end;
  end;
end;

function libr3r_get_user_agent: PChar; cdecl;
begin
  libr3r_get_user_agent := StrToPCharAlloc(UserAgent);
end;

procedure libr3r_set_user_agent_info(uainfo: PChar); cdecl;
var
  UA: String;
begin
  WriteStr(UA, uainfo);
  SetUserAgentInfo(uainfo);
end;

procedure libr3r_register_setting(setting_name, setting_section: PChar; setting_value: Pointer; setting_type: word; descr: PChar); cdecl;
var
  Name, Pri, Section: String;
  Value: String;
begin
  WriteStr(Name, setting_name);
  WriteStr(Pri, descr);
  WriteStr(Section, setting_section);

  case setting_type of
    TypeBoolean:
    begin
      Settings.RegisterBoolean(Name, Section, PtrUInt(setting_value) <> 0, Pri);
    end;
    TypeInteger:
    begin
      Settings.RegisterInteger(Name, Section, integer(setting_value), Pri);
    end;
    TypeString:
    begin
      WriteStr(Value, PChar(setting_value));
      Settings.RegisterString(Name, Section, Value, Pri);
    end;
  end;
end;

procedure libr3r_access_settings(var setting_name: PChar; var SettingValue: Pointer; var SettingType: byte; var Count: integer; const SettingsMode: byte); cdecl;
var
  SettingName: ShortString;
begin
  WriteStr(SettingName, setting_name);
  Settings.Access(SettingName, SettingValue, SettingType, Count, SettingsMode);
  setting_name := StrToPCharAlloc(SettingName);
end;

procedure libr3r_access_subscriptions(Index, Mode: byte; var Subscription: PChar; var Count: word); cdecl;
var
  sub: String;
begin
  Count := Subscriptions^.Count;

  WriteStr(sub, Subscription);

  if Mode = SubscriptionAdd then
  begin
    Subscriptions^.Add(sub);
  end
  else if Mode = SubscriptionDelete then
  begin
    if (Index = 0) and (Subscription <> nil) then
    begin
      Subscriptions^.DeleteString(sub);
    end
    else
    begin
      Subscriptions^.Delete(Index);
    end
  end
  else if Mode = SubscriptionGet then
  begin
    Subscription := StrToPCharAlloc(Subscriptions^.GetNth(Index));
  end;
end;

procedure libr3r_history_add(Entry: PChar); cdecl;
var
  Hent: String;
begin
  WriteStr(Hent, Entry);
  History^.Add(Hent);
end;

function libr3r_history_is_next: integer; cdecl;
begin
  libr3r_history_is_next := Ord(History^.IsNext);
end;

function libr3r_history_next: PChar; cdecl;
begin
  libr3r_history_next := StrToPCharAlloc(History^.GetNext);
end;

function libr3r_get_settings_dir: PChar; cdecl;
begin
  libr3r_get_settings_dir := StrToPCharAlloc(GetSettingsDir);
end;

function libr3r_get_data_dir: PChar; cdecl;
begin
  libr3r_get_data_dir := StrToPCharAlloc(GetDataDir);
end;

function libr3r_get_cache_dir: PChar; cdecl;
begin
  libr3r_get_cache_dir := StrToPCharAlloc(GetCacheDir);
end;

function libr3r_get_version: PChar; cdecl;
begin
  libr3r_get_version := AppVersion
end;

exports
  libr3r_queue_uri, libr3r_unqueue_uri,
  libr3r_retrieve_chunk, libr3r_retrieve_feed,
  libr3r_on_item_parsed, libr3r_on_message_received,
  libr3r_get_item_field,
  libr3r_get_user_agent, libr3r_set_user_agent_info,
  libr3r_register_setting, libr3r_access_settings,
  libr3r_access_subscriptions,
  libr3r_history_add, libr3r_history_is_next, libr3r_history_next,
  libr3r_get_settings_dir, libr3r_get_data_dir, libr3r_get_cache_dir,
  libr3r_get_version;

initialization

Lib := TLibR3R_Shared.Create;

finalization

Lib.Free;

end.
