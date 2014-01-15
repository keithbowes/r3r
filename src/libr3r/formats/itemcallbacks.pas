unit ItemCallbacks;

interface

uses
  FeedItem;

type
  TItemCallback = procedure(const Item: TFeedItem);

procedure RegisterItemCallback(const cb: TItemCallback);
procedure FreeItemCallback;
procedure CallItemCallback(Item: TFeedItem);

implementation

{ For some reason, using AnsiString causes a memory leak }
{$H-}

uses
  HttpCache, RFilter, RProp, RSettings;

procedure RegisterItemCallback(const cb: TItemCallback);
begin
  SetProp('item-callback', @cb);
end;

procedure FreeItemCallback;
begin
  RemoveProp('item-callback');
end;

procedure CallItemCallback(Item: TFeedItem);
var
  cb: TItemCallback;
  HideItems: Boolean;
  ShouldShow: Boolean = true;
begin
  cb := TItemCallback(GetProp('item-callback'));
  if Assigned(cb) then
  begin
    if Settings.GetBoolean('use-filters') then
    begin
      FilterItem(Item);
    end;

    if (Item.Id <> '') and Assigned(CurrentCache) then
    begin
      HideItems := Settings.GetBoolean('hide-cached-feed-items');
      HideItems := HideItems or Settings.GetBoolean('display-feed-title-only');
      CurrentCache.WriteData(Item.Id, cdtIds);

      if (CurrentCache.GetIdsList^.Count > 0) and
        (CurrentCache.GetIdsList^.IndexOf(Item.Id) <> -1) and HideItems then
      begin
        Item.Clear;
        ShouldShow := false;
      end;

      { Needed so that feed items will get displayed before they're cached }
      Item.Id := '';
    end;

    if ShouldShow then
    begin
      Item.Translate;
      cb(Item);
      Item.Clear;
    end
  end
end;

end.
