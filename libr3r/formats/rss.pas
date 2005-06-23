unit Rss;

interface

uses
  Feed, FeedItem, Xml;

type
  TRssFeed = class(TXmlFeed)
  protected
    function GetFormat: TFeedType; override;
  public
    procedure ParseLine(Line: String; var Item: TFeedItem; var ItemFinished: Boolean); override;
  end;

implementation

uses
  RSock;

procedure TRssFeed.ParseLine(Line: String; var Item: TFeedItem; var ItemFinished: Boolean);
begin
  inherited ParseLine(Line, Item, ItemFinished);
  //ItemFinished := (FXmlElement^.Name = 'item') or (Line = SockEof);
  if ItemFinished then Writeln('New item!');
end;

function TRssFeed.GetFormat: TFeedType;
begin
  Result := ftRss;
end;

end.
