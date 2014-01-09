unit DC;

{ Hackish Dublin Core support. }

interface

uses
  Feed, FeedItem, Xml;

const
  DCNS = 'http://purl.org/dc/elements/1.1/';

type
  TDCFeed = class(TXmlFeed)
  protected
    function GetFormat: TFeedType; override;
  public
    procedure ParseLine(Line: String; var Item: TFeedItem); override;
  end;

implementation

uses
  RDate;

procedure TDCFeed.ParseLine(Line: String; var Item: TFeedItem);
begin
  with Item, GetCurrentElement do
  begin
    if Name = 'title' then
    begin
      Title := Content;
    end
    else if Name = 'subject' then
    begin
      if Subject = '' then
      begin
        Subject := Content;
      end
      else if Content <> '' then
      begin
        Subject := Subject + ', ' + Content;
      end;
    end
    else if Name = 'description' then
    begin
      Description := Content;
    end
    else if Name = 'creator' then
    begin
      Contact.Name := Content;
    end
    else if Name = 'rights' then
    begin
      Copyright := Content;
    end
    else if Name = 'date' then
    begin
      Created := TimeToLongDate(ShortDateToTime(Content));
    end
    else if Name = 'identifier' then
    begin
      Id := Content;
    end;
  end;
  
  inherited ParseLine(Line, Item);
end;

function TDCFeed.GetFormat: TFeedType;
begin
  GetFormat := ftRss;
end;

end.
