unit Headers;

interface

uses
  Feed;

type
  THeaders = record
    ContentType: TFeedType;
    ContentEncoding: String;
    Date: String;
    Etag: String;
    Expires: String;
    LastModified: String;
    Status: word;
  end;

implementation

end.
