unit ItemCallbacks;

interface

uses
  FeedItem;

type
  PItemCallbackInfo = ^TItemCallbackInfo;
  TItemCallback = procedure(const Item: TFeedItem; const Data: Pointer);
  TItemCallbackInfo = record
    Callback: TItemCallback;
    Data: Pointer;
  end;

procedure RegisterItemCallback(const cb: TItemCallback; const Data: Pointer);
procedure FreeItemCallback;
procedure CallItemCallback(Item: TFeedItem);

implementation

{ For some reason, using AnsiString causes a memory leak }
{$H-}

uses
  HttpCache, RFilter, RProp, RSettings;

procedure RegisterItemCallback(const cb: TItemCallback; const Data: Pointer);
var
  ci: PItemCallbackInfo;
begin
  if Assigned(GetProp('item-callback')) then
  begin
    FreeItemCallback;
  end;

  New(ci);
  ci^.Callback := cb;
  ci^.Data := Data;
  SetProp('item-callback', ci);
end;

procedure FreeItemCallback;
var
  ci: PItemCallbackInfo;
begin
  ci := GetProp('item-callback');
  if Assigned(ci) then
  begin
    Dispose(ci);
  end;
end;

procedure CallItemCallback(Item: TFeedItem);
var
  ci: PItemCallbackInfo;
  CurrentCache: THttpCache;
  HideItems: Boolean;
  ShouldShow: Boolean = true;
begin
  ci := GetProp('item-callback');
  CurrentCache := THTtpCache(GetObjectProp(Item, 'cache'));

  if Assigned(ci) then
  begin
    if Settings.GetBoolean('use-filters') then
    begin
      FilterItem(Item);
    end;
    ShouldShow := not Item.Filtered;

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
      ci^.Callback(Item, ci^.Data);
      Item.Clear;
    end
  end
end;

end.
