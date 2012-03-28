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
  Elem: TXmlElement;
  i: byte;
begin
  inherited ParseLine(Line, Item);
  Elem := GetCurrentElement;
  StripNS(Elem.Name, Mod_EnclosureNS);

  with Elem, Item do
  begin
    if Elem.Name = 'enclosure' then
    begin
      for i := Low(Attributes) to High(Attributes) do
      begin
        StripNS(Attributes[i].Name, Mod_EnclosureNS);
        if Attributes[i].Name = 'resource' then
        begin
          Enclosure.URL := Attributes[i].Value;
        end
        else if Attributes[i].Name = 'type' then
        begin
          Enclosure.MimeType := Attributes[i].Value;
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
