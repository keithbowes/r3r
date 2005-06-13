unit Headers;

interface

uses
  Feed;

type
  THeaders = record
    ContentType: TFeedType;
    ContentEncoding: String;
    Date: String;
    Status: String;
  end;

implementation

end.
