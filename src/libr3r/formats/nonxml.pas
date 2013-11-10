unit NonXml;

interface

uses
  Feed, FeedItem;

type
  TNonXmlFeed = class(TFeed)
  protected
    FTopLink: String;
    function GetAbsoluteURL(const URL: String): String;
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
  Item.AllowsHTML := false;

  ShouldShow := (not ((Line = '') and (Line = LastLine))) and (Line <> SockEof);
  LastLine := Line;
end;

function TNonXmlFeed.GetAbsoluteURL(const URL: String): String;
begin
  GetAbsoluteURL := inherited GetAbsoluteURL(URL, FTopLink);
end;

end.
