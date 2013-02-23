unit Rss3Writer;

interface

uses
  Conv, FeedItem;

procedure WriteRss3(Item: TFeedItem);

implementation

procedure WriteRss3(Item: TFeedItem);
var
  fh: PText;
begin
  fh := GetFileHandle;
  WriteLn(fh^, 'title: ', Item.Title, LineEnding, 'link: ', Item.Link,
    LineEnding, 'description: ', Item.Description, LineEnding, 'subject: ',
    Item.Subject, LineEnding, 'generator: ', Item.Generator, LineEnding,
    'language: ', Item.Language, LineEnding, 'guid: ', Item.Id, LineEnding,
    'rights: ', Item.Copyright, LineEnding, LineEnding);

  if OutFile <> '' then
  begin
    Close(fh^);
  end;

  Dispose(fh);
end;

end.
