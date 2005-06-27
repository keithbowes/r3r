unit Rss;

interface

uses
  Feed, FeedItem, Xml;

type
  TRssFeed = class(TXmlFeed)
  protected
    function GetFormat: TFeedType; override;
    procedure FillItem(var Item: TFeedItem);
  public
    procedure ParseLine(Line: String; var Item: TFeedItem; var ItemFinished: Boolean); override;
  end;

implementation

uses
  RSock;

procedure TRssFeed.ParseLine(Line: String; var Item: TFeedItem; var ItemFinished: Boolean);
begin
  inherited ParseLine(Line, Item, ItemFinished);
  FillItem(Item);
  ItemFinished := (FXmlElement.Name = 'item') or (Line = SockEof);
end;

function TRssFeed.GetFormat: TFeedType;
begin
  Result := ftRss;
end;

procedure TRssFeed.FillItem(var Item: TFeedItem);
begin
  with FXmlElement do
  begin
    if Name = 'title' then
    begin
      Item.Title := Item.Title + Content;
    end
    else if Name = 'link' then
    begin
      Item.Links.Add(Content);
    end;
  end;
end;

end.
