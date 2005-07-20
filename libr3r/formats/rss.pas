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
  SockConsts;

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
      Item.LinksCount := Length(Item.Links);
      SetLength(Item.Links, Item.LinksCount + 1);
      Item.Links[Item.LinksCount] := Content;
    end
    else if Name = 'category' then
    begin
      Item.Subject := Item.Subject + Content;
    end
    else if Name = 'pubdate' then
    begin
      Item.Created := Item.Created + Content;
    end
    else if Name = 'managingeditor' then
    begin
      Item.Contact := CreateEmailRecord(Content, '(', 1);
    end
    else if Name = 'generator' then
    begin
      Item.Generator := Item.Generator + Content;
    end
    else if Name = 'lastpubdate' then
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
