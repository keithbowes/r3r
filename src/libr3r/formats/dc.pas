unit DC;

{ Hackish Dublin Core support.  I'm plannig on refactoring the
  XML code in beta 2, so I'm not going too far on this now. }

interface

uses
  Feed, FeedItem, Xml;

type
  TDCFeed = class(TXmlFeed)
  protected
    function GetFormat: TFeedType; override;
  public
    procedure ParseLine(Line: String; var Item: TFeedItem; var ItemFinished: Boolean); override;
  end;

implementation

uses
  RDate;

procedure TDCFeed.ParseLine(Line: String; var Item: TFeedItem; var ItemFinished: Boolean);
begin
  with Item, GetCurrentElement do
  begin
    StripNS(Name, DCNS);

    if Name = 'title' then
    begin
      Title := Content;
    end
    else if Name = 'subject' then
    begin
      Subject := Content;
    end
    else if Name = 'description' then
    begin
      Description := Content;
    end
    else if Name = 'creator' then
    begin
      Contact^.Toee := Content;
    end
    else if Name = 'rights' then
    begin
      Copyright := Content;
    end
    else if Name = 'date' then
    begin
      Created := TimeToString(ShortDateToTime(Content));
    end
    else if Name = 'identifier' then
    begin
      Id := Content;
    end;
  end;
  
  inherited ParseLine(Line, Item, ItemFinished);
end;

function TDCFeed.GetFormat: TFeedType;
begin
  GetFormat := ftRss;
end;

end.
