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
    else if Name = 'description' then
    begin
      Item.Description := Item.Description + Content;
    end
    else if Name = 'link' then
    begin
      Item.Links.Add(Content);
    end
    else if Name = 'category' then
    begin
      Item.Subject := Item.Subject + Content;
    end
    else if Name = 'pubDate' then
    begin
      Item.Created := Item.Created + Content;
    end
    else if Name = 'managingEditor' then
    begin
      Item.Contact := Item.Contact + Content;
    end
    else if Name = 'generator' then
    begin
      Item.Generator := Item.Generator + Content;
    end
    else if Name = 'lastPubDate' then
    begin
      Item.LastModified := Item.LastModified + Content;
    end
    else if Name = 'language' then
    begin
      Item.Language := Item.Language + Content;
      FXmlElement.Lang := Item.Language;
    end
    else if Name = 'copyright' then
    begin
      Item.Copyright := Item.Copyright + Content;
    end
    else if Name = 'guid' then
    begin
      Item.Id := Item.Id + Content;
      Item.Uri := Item.Id;
    end;
  end;
end;

end.
