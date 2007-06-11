unit Feed;

interface

uses
  FeedItem;

type
  TFeedType = (ftUnset, ftAtom, ftEsf, ftRss, ftRss3, ftUnknown);

  TFeed = class
  protected
    function GetFormat: TFeedType; virtual; abstract;
  public
    procedure ParseLine(Line: String; var Item: TFeedItem; var ItemFinished: Boolean); virtual; abstract;
    property Format: TFeedType read GetFormat;
  end; 

implementation

end.
