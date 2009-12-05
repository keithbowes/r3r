unit Xml;

interface

uses
  Feed, FeedItem,
  {$IFDEF SAX_LIBXML2}
    LibXml2
  {$ELSE}
    Expas
  {$ENDIF};

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
    FElemList: array of TXmlElement;
    FElems: cardinal;
    FIgnoreWhiteSpace: Boolean;
    {$IFDEF SAX_LIBXML2}
      FHandler: XmlSaxHandler;
    {$ELSE}
      FParser: XML_PARSER;
    {$ENDIF}
  protected
    FXmlElement: TXmlElement;
    function GetCurrentElement: TXmlElement;
    function GetPreviousElement: TXmlElement;
    procedure StripNS(var Element: String; const NS: String);
  public
    constructor Create;
    destructor Destroy; override;
    procedure ParseLine(Line: String; var Item: TFeedItem; var ItemFinished: Boolean); override;
    procedure Clone(const Element: TXmlElement);
    property CurrentElement: TXmlElement read GetCurrentElement;
    property PreviousElement: TXmlElement read GetPreviousElement;
  end;

implementation

procedure ElementStarted(user_data: Pointer; name: PChar; attrs: PPChar); cdecl; forward;
procedure ElementEnded(user_data: Pointer; name: PChar); cdecl; forward;
procedure CharactersReceived(ctx: Pointer; ch: PChar; len: Longint); cdecl; forward;
procedure WhitespaceReceived(ctx: Pointer; ch: PChar; len: Longint); cdecl; forward;

constructor TXmlFeed.Create;
begin
  inherited Create;

  SetLength(FElemList, 2);
  FIgnoreWhiteSpace := false;

  {$IFDEF SAX_LIBXML2}
    XmlInitParser;

    with FHandler do
    begin
      initialized := $DEEDBEAF; // Make sure we're using SAX 2
      startElement := @ElementStarted;
      endElement := @ElementEnded;
      characters := @CharactersReceived;
      ignorableWhitespace := @WhitespaceReceived;
    end;
  {$ELSE}
    FParser := XML_ParserCreateNS('UTF-8', #0);
    XML_SetElementHandler(FParser, @ElementStarted, @ElementEnded);
    XML_SetCharacterDataHandler(FParser, @CharactersReceived);
    XML_SetUserData(FParser, Self);
  {$ENDIF}
end;

procedure TXmlFeed.ParseLine(Line: String; var Item: TFeedItem; var ItemFinished: Boolean);
begin
  inherited ParseLine(Line, Item, ItemFinished);
  {$IFDEF SAX_LIBXML2}
    XmlSaxUserParseMemory(@FHandler, Self, PChar(Line), Length(Line));
  {$ELSE}
    XML_Parse(FParser, PChar(Line), Length(Line), 0);
  {$ENDIF}

  ItemFinished := true;
end;

procedure TXmlFeed.Clone(const Element: TXmlElement);
begin
  FXmlElement := Element;
end;

destructor TXmlFeed.Destroy;
begin
  FElemList := nil;

  {$IFDEF SAX_LIBXML2}
    XmlCleanupParser;
  {$ELSE}
    Xml_ParserFree(FParser);
  {$ENDIF}

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
var
  Len: word;
begin
  if Pos(NS, Element) = 1 then
  begin
    Len := Length(NS);
    Delete(Element, 1, Len);
  end;
end;

procedure ElementStarted(user_data: Pointer; name: PChar; attrs: PPChar); cdecl;
var
  attr: String;
  i, j: word;
begin
  i := 0;
  j := 0;

  with TXmlFeed(user_data) do
  begin
    FXmlElement.Name := name;
    FXmlElement.Content := '';

    if Assigned(attrs) then
    begin
      while Assigned(attrs[i]) do
      begin
        attr := String(attrs[i]);
        StripNS(attr, XMLNSNS);
        attrs[i] := PChar(attr);

        if attrs[i] = 'base' then
        begin
          FXmlElement.Base := attrs[i + 1];
        end
        else if attrs[i] = 'lang' then
        begin
          FXmlElement.Lang := attrs[i + 1];
        end
        else if j < 11 then
        begin
          FXmlElement.Attributes[j].Name := attrs[i];
          FXmlElement.Attributes[j].Value := attrs[i + 1];
          Inc(j);
        end;

        Inc(i, 2);
      end;
    end;

    FElems := Length(FElemList);
    SetLength(FElemList, FElems + 1);
    FElemList[FElems] := FXmlElement;
  end;
end;

procedure ElementEnded(user_data: Pointer; name: PChar); cdecl;
begin
  with TXmlFeed(user_data) do
  begin
    FElems := Length(FElemList);
    if FElems > 0 then
    begin
      SetLength(FElemList, FElems - 1);
    end;

    FXmlElement.Name := name;
  end;
end;

procedure CharactersReceived(ctx: Pointer; ch: PChar; len: Longint); cdecl;
var
  enh: String;
begin
  enh := Copy(ch, 1, len);

  with TXmlFeed(ctx) do
  begin
    FXmlElement.Content := FXmlElement.Content + enh;
  end;
end;

procedure WhitespaceReceived(ctx: Pointer; ch: PChar; len: Longint); cdecl;
begin
  with TXmlFeed(ctx) do
  begin
    if not FIgnoreWhiteSpace then
    begin
      FXmlElement.Content := FXmlElement.Content + ch;
    end;
  end;
end;

end.
