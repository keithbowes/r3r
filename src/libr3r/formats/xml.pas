unit Xml;

interface

uses
  Expas, Feed, FeedItem;

const
  AtomNS = 'http://www.w3.org/2005/Atom';
  DCNS = 'http://purl.org/dc/elements/1.1/';
  RDFNS = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#';
  RSS1NS = 'http://purl.org/rss/1.0/';
  XMLNSNS = 'http://www.w3.org/XML/1998/namespace';

type
  TXmlAttr = record
    Name: String;
    Value: String;
  end;

  TXmlElement = record
    Name: String;
    { Yeah, like anybody will exceed 11 attributes per element }
    Attributes: array [0..10] of TXmlAttr;
    Content: String;
    Base: String;
    Lang: String;
  end;

  TXmlFeed = class(TFeed)
  private
    FParser: XML_PARSER;
  protected
    function GetCurrentElement: TXmlElement;
    function GetPreviousElement: TXmlElement; virtual;
  public
    FElemList: array of TXmlElement;
    FElems: cardinal;
    FXmlElement: TXmlElement;
    constructor Create;
    destructor Destroy; override;
    procedure ParseLine(Line: String; var Item: TFeedItem; var ItemFinished: Boolean); override;
    procedure StripNS(var Element: String; const NS: String);
    procedure Clone(const Element: TXmlElement);
    property CurrentElement: TXmlElement read GetCurrentElement;
    property PreviousElement: TXmlElement read GetPreviousElement;
  end;

implementation

uses
  SaxCallbacks;

constructor TXmlFeed.Create;
begin
  inherited Create;

  SetLength(FElemList, 2);

  FParser := XML_ParserCreateNS('UTF-8', #0);
  XML_SetElementHandler(FParser, @ElementStarted, @ElementEnded);
  XML_SetCharacterDataHandler(FParser, @CharactersReceived);
  XML_SetUserData(FParser, Self);
end;

procedure TXmlFeed.ParseLine(Line: String; var Item: TFeedItem; var ItemFinished: Boolean);
begin
  inherited ParseLine(Line, Item, ItemFinished);

  XML_Parse(FParser, PChar(Line), Length(Line), 0);
  ItemFinished := true;
end;

{ Clones an element.  This is necessary for namespace support. }
procedure TXmlFeed.Clone(const Element: TXmlElement);
begin
  FXmlElement := Element;
end;

destructor TXmlFeed.Destroy;
begin
  FElemList := nil;
  Xml_ParserFree(FParser);

  inherited Destroy;
end;

function TXmlFeed.GetCurrentElement: TXmlElement;
begin
  FElems := Length(FElemList);
  Result := FElemList[FElems];
end;

function TXmlFeed.GetPreviousElement: TXmlElement;
begin
  FElems := Length(FElemList);
  Result := FElemList[FElems - 1];
end;

procedure TXmlFeed.StripNS(var Element: String; const NS: String);
begin
  if Pos(NS, Element) = 1 then
  begin
    Delete(Element, 1, Length(NS));
  end;
end;

end.
