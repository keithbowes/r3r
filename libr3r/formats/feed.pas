unit Feed;

interface

uses
  FeedItem, RegExpr;

type
  TFeedType = (ftUnset, ftAtom, ftEsf, ftRss, ftRss3, ftUnknown);

  TFeed = class
  protected
    FRegExpr: TRegExpr;
    function GetFormat: TFeedType; virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;
    function ParseLine(Line: String; var Item: TFeedItem): Boolean; virtual; abstract;
    property Format: TFeedType read GetFormat;
  end; 

implementation

constructor TFeed.Create;
begin
  inherited Create;
  FRegExpr := TRegExpr.Create;
end;

destructor TFeed.Destroy;
begin
  FRegExpr.Free;
  inherited Destroy;
end;

end.
