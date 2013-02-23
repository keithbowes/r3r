unit EsfWriter;

interface

uses
  Conv, FeedItem;

procedure WriteEsf(Item: TFeedItem; IsTop: Boolean);

implementation

const
  Tab = #9;

procedure WriteEsf(Item: TFeedItem; IsTop: Boolean);
var
  fh: PText;
begin
  fh := GetFileHandle;

  if IsTop then
  begin
    WriteLn(fh^, 'title', Tab, Item.Title);
    WriteLn(fh^, 'description', Tab, Item.Description);
    WriteLn(fh^, 'link', Tab, Item.Link);
    WriteLn(fh^, 'contact', Tab, Item.Contact.Email);
    WriteLn(fh^);
  end
  else
  begin
    WriteLn(fh^, FormatTime(Item.Created, dfUnix), Tab, Item.Title, Tab, Item.Link, Tab);
  end;

  if OutFile <> '' then
  begin
    Close(fh^);
  end;

  Dispose(fh);
end;

end.
