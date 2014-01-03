unit Mod_Enclosure;

interface

uses
  Feed, FeedItem, Xml;

const
  Mod_EnclosureNS = 'http://purl.oclc.org/net/rss_2.0/enc#';

type
  TModEnclosure = class(TXmlFeed)
  protected
    function GetFormat: TFeedType; override;
  public
    procedure ParseLine(Line: String; var Item: TFeedItem); override;
  end;

implementation

procedure TModEnclosure.ParseLine(Line: String; var Item: TFeedItem);
var
  Attr: TXmlAttr;
  Elem: TXmlElement;
  i: PtrUInt;
begin
  ShouldShow := false;
  inherited ParseLine(Line, Item);
  Elem := GetCurrentElement;

  with Elem, Item do
  begin
    if Elem.Name = 'enclosure' then
    begin
      for i := 0 to Attributes^.Count - 1 do
      begin
        Attr := PXmlAttr(Attributes^.GetNth(i))^;
        if Attr.Name = 'resource' then
        begin
          Enclosure.URL := GetAbsoluteURL(Attr.Value);
        end
        else if Attr.Name = 'type' then
        begin
          Enclosure.MimeType := Attr.Value;
        end;
      end;
    end;
  end;
end;

function TModEnclosure.GetFormat: TFeedType;
begin
  GetFormat := ftRss;
end;

end.
