unit FeedItem;

interface

uses
  Classes;

type
  TFeedItem = record
    Title: String;
    Links: TStringList;
    Description: String;
    Subject: String;
    Created: String;
    Contact: String;
    Generator: String;
    LastModified: String;
    Language: String;
    Copyright: String;
    Id: String;
    Uri: String;
  end;

implementation

end.
