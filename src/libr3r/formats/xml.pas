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
    FElemList: array of TXmlElement;
    FElems: cardinal;
    FParser: XML_PARSER;
  protected
    FXmlElement: TXmlElement;
    function GetCurrentElement: TXmlElement;
    function GetPreviousElement: TXmlElement; virtual;
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

end.
