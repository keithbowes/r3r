unit Feed;

interface

uses
  FeedItem;

type
  TFeedType = (ftUnset, ftXml, ftAtom, ftEsf, ftRss, ftRss3, ftUnknown);

  TFeed = class
  private
    FFormat: TFeedType;
  protected
    function GetFormat: TFeedType; virtual; abstract;
  public
    procedure ParseLine(Line: String; var Item: TFeedItem; var ItemFinished: Boolean); virtual;
    property Format: TFeedType read GetFormat write FFormat;
  end; 

implementation

uses
  Headers, HttpCache, RSettings;

procedure TFeed.ParseLine(Line: String; var Item: TFeedItem; var ItemFinished: Boolean);
var
  HideItems: Boolean;
begin
  { Don't cache local feeds }
  if CurrentCache <> nil then
  begin
    HideItems := Settings.GetBoolean(Settings.IndexOf('hide-cached-feed-items'));
    HideItems := HideItems or Settings.GetBoolean(Settings.IndexOf('display-feed-title-only'));

    with CurrentCache do
    begin

      Info^.HeaderRec.ContentType := Format;
      WriteData(Line, cdtFeed);

      if Item.Id <> '' then
      begin
        if (IdsList.IndexOf(Item.Id) <> -1) and HideItems then
        begin
          Item.Title := '';
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
