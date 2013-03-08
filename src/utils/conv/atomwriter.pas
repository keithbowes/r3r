unit AtomWriter;

interface

uses
  Conv, FeedItem;

procedure WriteAtom(Item: TFeedItem; IsTop: Boolean);
procedure CloseAtom;

implementation

uses
  SysUtils;

procedure SanitizeURL(var URL: String);
begin
  URL := StringReplace(URL, '&', '&amp;', [rfReplaceAll]);
end;

procedure WriteAtom(Item: TFeedItem; IsTop: Boolean);
const
  erste: Boolean = true;
var
  Created, Modified: String;
  fh: PText;
begin
  fh := Conv.GetFileHandle;

  if erste then
  begin
    WriteLn(fh^, '<?xml version="1.0"?><feed version="1.0" xmlns="http://www.w3.org/2005/Atom" xml:lang="', Item.Language, '">');
    erste := false;
  end;

  if not IsTop then
  begin
    WriteLn(fh^, '<entry xml:lang="', Item.Language, '">', LineEnding);
  end;

  Created := Conv.FormatTime(Item.Created, dfShort);
  Modified := Conv.FormatTime(Item.LastModified, dfShort);

  SanitizeURL(Item.Link);
  SanitizeURL(Item.Enclosure.URL);
  WriteLn(fh^, '<title><![CDATA[', Item.Title, ']]></title>', LineEnding,
    '<link rel="alternate" href="', Item.Link, '"/>', LineEnding,
    '<link rel="self" href="', Item.MySelf, '"/>', LineEnding,
    '<link rel="enclosure" href="', Item.Enclosure.URL, '"/>', LineEnding,
    '<content type="html">', LineEnding, '<![CDATA[', Item.Description, ']]>',
    LineEnding, '</content>', LineEnding, '<category label="',
    Item.Subject, '"/>', LineEnding, '<published>', Created, '</published>',
    LineEnding, '<updated>', Modified, '</updated>', LineEnding, '<author>',
    LineEnding, '<name><![CDATA[', Item.Contact.Name, ']]></name>', LineEnding,
    '<email>', Item.Contact.Email, '</email>', LineEnding, '</author>',
    LineEnding, '<generator><![CDATA[', Item.Generator, ']]></generator>',
    LineEnding, '<id>', Item.Id, '</id>', LineEnding, '<copyright><![CDATA[',
    Item.Copyright, ']]></copyright>', LineEnding);

  if not IsTop then
  begin
    WriteLn(fh^, '</entry>', LineEnding);
  end;

  Close(fh^);
  Dispose(fh);
end;

procedure CloseAtom;
var
  fh: PText;
begin
  fh := Conv.GetFileHandle;
  WriteLn(fh^, '</feed>');
  
  if OutFile <> '' then
  begin
    Close(fh^);
  end;

  Dispose(fh);
end;

end.
