unit EsfWriter;

interface

uses
  Conv, FeedItem, SysUtils;

procedure WriteEsf(Item: TFeedItem; IsTop: Boolean);

implementation

const
  Tab = #9;

procedure WriteEsf(Item: TFeedItem; IsTop: Boolean);
var
  fh: PText;
begin
  fh := Conv.GetFileHandle;

  if IsTop then
  begin
    WriteLn(fh^, 'title', Tab, Trim(Item.Title));
    WriteLn(fh^, 'description', Tab, Trim(Item.Description));
    WriteLn(fh^, 'link', Tab, Trim(Item.Link));
    WriteLn(fh^, 'contact', Tab, Trim(Item.Contact.Email));
    WriteLn(fh^);
  end
  else
  begin
    WriteLn(fh^, FormatTime(Trim(Item.Created), dfUnix), Tab, Trim(Item.Title), Tab, Trim(Item.Link));
  end;

  if OutFile <> '' then
  begin
    Close(fh^);
  end;

  Dispose(fh);
end;

end.
