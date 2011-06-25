unit ItemCallbacks;

{$MODE delphi}

interface

uses
  FeedItem;

type
  TItemCallback = procedure(const Item: TFeedItem);

procedure RegisterItemCallback(const cb: TItemCallback);
procedure FreeItemCallback;
procedure CallItemCallback(Item: TFeedItem);

implementation

uses
  RFilter, RProp, RSettings;

procedure RegisterItemCallback(const cb: TItemCallback);
begin
  SetProp('item-callback', @cb);
end;

procedure FreeItemCallback;
begin
  RemoveProp('item-callback');
end;

procedure CallItemCallback(Item: TFeedItem);
const
  LastTitle: String = '';
var
  cb: TItemCallback;
begin
  cb := TItemCallback(GetProp('item-callback'));
  if Assigned(cb) then
  begin
    if (Item.Title <> '') and (Item.Title <> LastTitle) then
    begin
      Item.Translate;

      if Settings.GetBoolean(Settings.IndexOf('use-filters')) then
      begin
        FilterItem(Item);
      end;

      cb(Item);
      LastTitle := Item.Title;
    end;
  end;
end;

end.
