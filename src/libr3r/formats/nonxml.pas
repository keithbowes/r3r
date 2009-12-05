unit NonXml;

interface

uses
  Feed, FeedItem;

type
  TNonXmlFeed = class(TFeed)
  public
    procedure ParseLine(Line: String; var Item: TFeedItem; var ItemFinished: Boolean); override;
  end; 

implementation

uses
  SockConsts, SysUtils;

procedure TNonXmlFeed.ParseLine(Line: String; var Item: TFeedItem; var ItemFinished: Boolean);
const
  LastLine: String = '';
begin
  inherited ParseLine(Line, Item, ItemFinished);

  ShouldShow := (not ((Line = '') and (Line = LastLine))) and (Line <> SockEof);
  LastLine := Line;
end;

end.
