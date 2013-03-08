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

  Created := FormatTime(Item.Created, dfLong);
  Modified := FormatTime(Item.LastModified, dfLong);

  WriteLn(fh^, '<title><![CDATA[', Item.Title, ']]></title>', LineEnding,
    '<link><![CDATA[', Item.Link, ']]></link>', LineEnding,
    '<description><![CDATA[', LineEnding, Item.Description, LineEnding, ']]>',
    LineEnding, '</description>', LineEnding, '<category><![CDATA[',
    Item.Subject, ']]></category>', LineEnding, '<pubDate>', Created,
    '</pubDate>', LineEnding, '<lastPubDate>', Modified, '</lastPubDate>',
    LineEnding, '<generator><![CDATA[', Item.Generator, ']]></generator>',
    LineEnding, '<guid><![CDATA[', Item.Id, ']]></guid>', LineEnding,
    '<language>', Item.Language, '</language>', LineEnding,
    '<copyright><![CDATA[', Item.Copyright, ']]></copyright>', LineEnding,
    '<atom:link rel="self" href="', Item.MySelf, '"/>', LineEnding,
    '<enclosure><![CDATA[', Item.Enclosure.URL, ']]></enclosure>', LineEnding);

  if not IsTop then
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
