unit Xml;

interface

uses
  Classes, Feed, FeedItem, Sax, Sax_Html;

type
  TXmlFeed = class(TFeed)
  protected
    FElem: SaxString;
    FInput: TSaxInputSource;
    FReader: THtmlReader;
    FStream: TStringStream;
    procedure ElementStarted(Sender: TObject; const NamespaceURI, LocalName, QName: SAXString; Atts: TSAXAttributes);
  public
    constructor Create;
    destructor Destroy; override;
    function ParseLine(Line: String; var Item: TFeedItem): Boolean; override;
  end;

implementation

constructor TXmlFeed.Create;
begin
  inherited Create;

  FStream := TStringStream.Create('');
  FInput := TSaxInputSource.Create(FStream);
  FReader := THtmlReader.Create;
  FReader.OnStartElement := @ElementStarted;
end;

function TXmlFeed.ParseLine(Line: String; var Item: TFeedItem): Boolean;
begin
  FStream.Free;

  FStream := TStringStream.Create(Line);
  FReader.Parse(FInput);

  Result := true;
end;

destructor TXmlFeed.Destroy;
begin
  FStream.Free;
  FInput.Free;
  FReader.Free;

  inherited Destroy;
end;

procedure TXmlFeed.ElementStarted(Sender: TObject; const NamespaceURI, LocalName, QName: SAXString; Atts: TSAXAttributes);
begin
  WriteLn('a', LocalName);
  FElem := LocalName;
end;

end.
