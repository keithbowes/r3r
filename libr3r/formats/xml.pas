unit Xml;

interface

uses
  Classes, Feed, FeedItem, Sax, Sax_Html;

type
  PXmlElement = ^TXmlElement;
  TXmlElement = record
    Base: String;
    Name: String;
    Lang: String;
    Prev: PXmlElement;
  end;

  TXmlFeed = class(TFeed)
  private
    FInput: TSaxInputSource;
    FReader: THtmlReader;
    FStream: TStringStream;
  protected
    FXmlElement: PXmlElement;
    procedure ElementStarted(Sender: TObject; const NamespaceURI, LocalName, QName: SAXString; Atts: TSAXAttributes);
    procedure ElementEnded(Sender: TObject; const NamespaceURI, LocalName, QName: SAXString);
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
  FReader.onEndElement := @ElementEnded;
  New(FXmlElement);
end;

function TXmlFeed.ParseLine(Line: String; var Item: TFeedItem): Boolean;
begin
  FStream.Free;

  FStream := TStringStream.Create(Line);
  FReader.Parse(FInput);

  Result := false;
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
  New(FXmlElement^.Prev);
  FXmlElement^.Prev := FXmlElement;
  FXmlElement^.Name := LocalName;
end;

procedure TXmlFeed.ElementEnded(Sender: TObject; const NamespaceURI, LocalName, QName: SAXString);
begin
  Writeln(FXmlElement^.Name);
  FXmlElement := FXmlElement^.Prev;
end;

end.
