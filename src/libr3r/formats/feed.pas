unit Feed;

interface

uses
  FeedItem;

type
  TFeedType = (ftUnset, ftXml, ftAtom, ftDc, ftEsf, ftRss, ftRss3, ftUnknown);

  TFeed = class
  private
    FFormat: TFeedType;
    FShouldShow: Boolean;
  protected
    constructor Create;
    function GetFormat: TFeedType; virtual; abstract;
  public
    procedure ParseLine(Line: String; var Item: TFeedItem; var ItemFinished: Boolean); virtual;
    property Format: TFeedType read GetFormat write FFormat;
    property ShouldShow: Boolean read FShouldShow write FShouldShow;
  end; 

implementation

uses
  Headers, HttpCache, RSettings, SockConsts;

constructor TFeed.Create;
begin
  inherited Create;
  ShouldShow := true;
end;

procedure TFeed.ParseLine(Line: String; var Item: TFeedItem; var ItemFinished: Boolean);
var
  HideItems: Boolean;
begin
  { Don't cache local files }
  if CurrentCache <> nil then
  begin
    HideItems := Settings.GetBoolean(Settings.IndexOf('hide-cached-feed-items'));
    HideItems := HideItems or Settings.GetBoolean(Settings.IndexOf('display-feed-title-only'));

    with CurrentCache do
    begin
      Info^.HeaderRec.ContentType := Format;

      if Line <> SockEof then
      begin
        WriteData(Line, cdtFeed);
      end;

      if Item.Id <> '' then
      begin
        if (IdsList.IndexOf(Item.Id) <> -1) and HideItems then
        begin
          Item.Clear;
          ShouldShow := false;
        end;

        if HideItems then
        begin
          WriteData(Item.Id, cdtIds);
        end;

        Item.Id := '';
      end;
    end;
  end;
end;

end.
