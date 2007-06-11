unit NonXml;

interface

uses
  Feed, RegExpr;

type
  TNonXmlFeed = class(TFeed)
  protected
    FRegExpr: TRegExpr;
  public
    constructor Create;
    destructor Destroy; override;
  end; 

implementation

constructor TNonXmlFeed.Create;
begin
  inherited Create;
  FRegExpr := TRegExpr.Create;
end;

destructor TNonXmlFeed.Destroy;
begin
  FRegExpr.Free;
  inherited Destroy;
end;

end.
