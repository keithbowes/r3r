unit NonXml;

interface

uses
  Feed, FeedItem;

type
  TNonXmlFeed = class(TFeed)
  private
    FLastLine: String;
  public
    procedure ParseLine(Line: String; var Item: TFeedItem); override;
  end; 

implementation

uses
  SockConsts;

procedure TNonXmlFeed.ParseLine(Line: String; var Item: TFeedItem);
begin
  inherited ParseLine(Line, Item);

  ShouldShow := (not ((Line = '') and (Line = FLastLine))) and (Line <> SockEof);
  FLastLine := Line;
end;

end.
