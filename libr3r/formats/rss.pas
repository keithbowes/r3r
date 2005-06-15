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

uses
  RSock;

function TRssFeed.ParseLine(Line: String; var Item: TFeedItem): Boolean;
begin
  inherited ParseLine(Line, Item);
  Result := (FXmlElement^.Name <> 'item') and (Line <> SockEof);
  if not Result then Writeln('New item!');
end;

function TRssFeed.GetFormat: TFeedType;
begin
  Result := ftRss;
end;

end.
