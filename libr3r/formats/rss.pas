unit Rss;

interface

uses
  Feed, FeedItem, Xml;

type
  TRssFeed = class(TXmlFeed)
  protected
    function GetFormat: TFeedType; override;
  public
    function ParseLine(Line: String; var Item: TFeedItem): Boolean; override;
  end;

implementation

function TRssFeed.ParseLine(Line: String; var Item: TFeedItem): Boolean;
begin
  inherited ParseLine(Line, Item);
  Writeln(FElem);
end;

function TRssFeed.GetFormat: TFeedType;
begin
  Result := ftRss;
end;

end.
