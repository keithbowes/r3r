unit Headers;

interface

uses
  Feed;

type
  TContentEncoding = (ceNone, ceBz2, ceDeflate, ceGzip, ceLzo, ceUnsupported);
  THeaders = record
    ContentType: TFeedType;
    ContentEncoding: TContentEncoding;
    Date: String;
    Etag: String;
    Expires: String;
    LastModified: String;
    Status: word;
  end;

implementation

end.
