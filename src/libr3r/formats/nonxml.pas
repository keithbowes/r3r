unit NonXml;

interface

uses
  Feed, FeedItem;

type
  TNonXmlFeed = class(TFeed)
  public
    procedure ParseLine(Line: String; var Item: TFeedItem); override;
  end; 

implementation

uses
  SockConsts;

procedure TNonXmlFeed.ParseLine(Line: String; var Item: TFeedItem);
const
  LastLine: String = '';
begin
  inherited ParseLine(Line, Item);

  ShouldShow := (not ((Line = '') and (Line = LastLine))) and (Line <> SockEof);
  LastLine := Line;
end;

end.
