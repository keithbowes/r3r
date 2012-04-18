unit Feed;

interface

uses
  FeedItem;

type
  TFeedType = (ftUnset, ftXml, ftAtom, ftEsf, ftRss, ftRss3, ftUnknown);

  TFeed = class
  protected
    function GetFormat: TFeedType; virtual; abstract;
  public
    ShouldShow: Boolean;
    constructor Create;
    destructor Destroy; {$IFNDEF __GPC__}override{$ELSE}virtual{$ENDIF};
    procedure ParseLine(Line: String; var Item: TFeedItem); virtual;
  end; 

implementation

uses
  HttpCache, RSettings, SockConsts;

constructor TFeed.Create;
begin
{$IFNDEF __GPC__}
  inherited Create;
{$ENDIF}

  ShouldShow := true;
end;

destructor TFeed.Destroy;
begin
{$IFNDEF __GPC__}
  inherited Destroy;
{$ENDIF}
end;

procedure TFeed.ParseLine(Line: String; var Item: TFeedItem);
var
  HideItems: Boolean;
begin
  { Don't cache local files }
  if CurrentCache <> nil then
  begin
    HideItems := Settings.GetBoolean('hide-cached-feed-items');
    HideItems := HideItems or Settings.GetBoolean('display-feed-title-only');

    with CurrentCache do
    begin
      Info^.HeaderRec.ContentType := GetFormat;

      if Line <> SockEof then
      begin
        WriteData(Line, cdtFeed);
      end;

      if Item.Id <> '' then
      begin
        if HideItems then
        begin
          WriteData(Item.Id, cdtIds);
        end;

        if (GetIdsList^.Count > 0) and
          (GetIdsList^.IndexOf(Item.Id) <> -1) and HideItems then
        begin
          Item.Clear;
          ShouldShow := false;
        end;

        { Needed so that feed items will get displayed before they're cached }
        Item.Id := '';
      end;
    end;
  end;
end;

end.
