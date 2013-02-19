unit AtomWriter;

interface

uses
  Conv, FeedItem;

procedure WriteAtom(Item: TFeedItem; IsTop: Boolean);
procedure CloseAtom;

implementation

procedure WriteAtom(Item: TFeedItem; IsTop: Boolean);
const
  erste: Boolean = true;
var
  Created, Modified: String;
  fh: PText;
begin
	fh := GetFileHandle;

	if erste then
	begin
		WriteLn(fh^, '<?xml version="1.0"?><feed version="1.0" xmlns="http://www.w3.org/2005/Atom" xml:lang="', Item.Language, '">');
    erste := false;
  end;

	if not IsTop then
  begin
		WriteLn(fh^, '<entry xml:lang="', Item.Language, '">', LineEnding);
  end;

  Created := FormatTime(Item.Created, 'yyyy-mm-ddThh:nn:ssZ');
  Modified := FormatTime(Item.LastModified, 'yyyy-mm-ddThh:nn:ssZ');

	WriteLn(fh^, '<title>', Item.Title, '</title>', LineEnding,
    '<link rel="alternate" href="', Item.Link, '"/>', LineEnding,
    '<link rel="self" href="', Item.MySelf, '"/>', LineEnding,
    '<link rel="enclosure" href="', Item.Enclosure.URL, '"/>', LineEnding,
    '<content type="html">', LineEnding, '<![CDATA[', Item.Description, ']]>',
    LineEnding, '</content>', LineEnding, '<category label="', Item.Subject,
    '"/>', LineEnding, '<created>', Created, '</created>', LineEnding,
    '<modified>', Modified, '</modified>', LineEnding, '<author>', LineEnding,
    '<name>', Item.Contact.Name, '</name>', LineEnding, '<email>',
    Item.Contact.Email, '</email>', LineEnding, '</author>', LineEnding,
    '<generator>', Item.Generator, '</generator>', LineEnding, '<id>',
    Item.Id, '</id>', LineEnding, '<copyright>',  Item.Copyright,
    '</copyright>', LineEnding);

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
	fh := GetFileHandle;
	WriteLn(fh^, '</feed>');
	
	if OutFile = '' then
  begin
		Close(fh^);
  end;

  Dispose(fh);
end;

end.
