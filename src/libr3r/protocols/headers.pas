unit Headers;

interface

uses
  Feed;

type
  THeaders = record
    Charset: String;
    ContentType: TFeedType;
    Date: String;
    Etag: String;
    Expires: String;
    LastModified: String;
    Message: String;
    Sniff: Boolean;
    Status: word;
  end;

implementation

end.
