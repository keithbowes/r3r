unit NonXml;

interface

uses
  Feed, FeedItem;

type
  TNonXmlFeed = class(TFeed)
  protected
    FTopLink: String;
  public
    function GetAbsoluteURL(const URL: String): String;
    procedure ParseLine(Line: String; var Item: TFeedItem); override;
  end; 

implementation

uses
  SockConsts;

procedure TNonXmlFeed.ParseLine(Line: String; var Item: TFeedItem);
begin
  inherited ParseLine(Line, Item);
  Item.AllowsHTML := false;

  ShouldShow := (Line <> '') and (Line <> SockEof);
end;

function TNonXmlFeed.GetAbsoluteURL(const URL: String): String;
begin
  GetAbsoluteURL := inherited GetAbsoluteURL(URL, FTopLink);
end;

end.
