unit RssWriter;

interface

uses
  Conv, FeedItem;

procedure WriteRss(Item: TFeedItem; IsTop: Boolean);
procedure CloseRss;

implementation

procedure WriteRss(Item: TFeedItem; IsTop: Boolean);
const
	erste: Boolean = true;
var
  Created, Modified: String;
  fh: PText;
begin
  fh := GetFileHandle;

	if erste then
	begin
		WriteLn(fh^, '<?xml version="1.0"?>', LineEnding, '<rss version="2.0" xmlns:atom="http://www.w3.org/2005/Atom">',
      LineEnding, '<channel>', LineEnding);
    erste := false;
  end;

	if not IsTop then
  begin
		WriteLn(fh^, '<item>', LineEnding);
  end;

  Created := FormatTime(Item.Created, 'ddd"," dd mmm yyyy hh:nn:ss');
  Modified := FormatTime(Item.LastModified, 'ddd"," dd mmm yyyy hh:nn:ss');

	WriteLn(fh^, '<title>', Item.Title, '</title>', LineEnding, '<link>',
    Item.Link, '</link>', LineEnding, '<description><![CDATA[', LineEnding,
    Item.Description, LineEnding, ']]>', LineEnding, '</description>',
    LineEnding, '<category>', Item.Subject, '</category>', LineEnding,
    '<pubDate>', Created, '</pubDate>', LineEnding, '<lastPubDate>', Modified,
    '</lastPubDate>', LineEnding, '<generator>', Item.Generator,
    '</generator>', LineEnding, '<guid>', Item.Id, '</guid>', LineEnding,
    '<language>', Item.Language, '</language>', LineEnding, '<copyright>',
    Item.Copyright, '</copyright>', LineEnding, '<atom:link rel="self" href="',
    Item.MySelf, '"/>', LineEnding, '<enclosure>', Item.Enclosure.URL,
    '</enclosure>', LineEnding);

	if IsTop then
  begin
		WriteLn(fh^, '</item>', LineEnding);
  end;

	if OutFile <> '' then
  begin
		Close(fh^);
  end;

  Dispose(fh);
end;

procedure CloseRss;
var
  fh: PText;
begin
  fh := GetFileHandle;
	WriteLn(fh^, '</channel>', LineEnding, '</rss>', LineEnding);
	
  if OutFile <> '' then
  begin
    Close(fh^);
  end;

  Dispose(fh);
end;

end.
