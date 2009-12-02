unit DC;

{ Hackish Dublin Core support.  I'm plannig on refactoring the
  XML code in beta 2, so I'm not going too far on this now. }

interface

uses
  FeedItem;

procedure DCFill(var Item: TFeedItem; const Element, Content: String);

implementation

uses
  SysUtils;

{ This assumes that Dublin Core has the dc: namespace prefix.
  It will be better when proper namespaces come into the code
  in beta 2. }
procedure DCFill(var Item: TFeedItem; const Element, Content: String);
var
  DT: TDateTime;
begin
  if Element = 'dc:title' then
  begin
    Item.Title := Content;
  end
  else if Element = 'dc:subject' then
  begin
    Item.Subject := Content;
  end
  else if Element = 'dc:description' then
  begin
    Item.Description := Content;
  end
  else if Element = 'dc:creator' then
  begin
    Item.Contact^.Toee := Content;
  end
  else if Element = 'dc:rights' then
  begin
    Item.Copyright := Content;
  end
  else if Element = 'dc:date' then
  begin
    ShortDateFormat := 'YYY-MM-DD';
    DT := StrToDateTime(Content);
    Item.Created := FormatDateTime('DD MMMM YYYY hh:nn', DT);
  end
  else if Element = 'dc:identifier' then
  begin
    Item.Id := Content;
  end;
end;

end.
