unit Xml;

interface

uses
  Expas, Feed, FeedItem, RList;

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

  PXmlElement = ^TXmlELement;
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
    FCloned: Boolean;
    FLastBase: String;
    FLastLang: String;
    FParser: XML_PARSER;
  public
    FElemList: PRList;
    FElems: cardinal;
    constructor Create; {$IFDEF __GPC__}override;{$ENDIF}
    procedure ParseLine(Line: String; var Item: TFeedItem; var ItemFinished: Boolean); override;
    destructor Destroy; override;
    procedure StripNS(var Element: String; const NS: String);
    procedure Clone(const List: PRList);
    function GetCurrentElement: TXmlElement; virtual;
    function GetPreviousElement: TXmlElement; virtual;
  end;

implementation

uses
  RStrings, SaxCallbacks;

constructor TXmlFeed.Create;
begin
{$IFNDEF __GPC__}
  inherited Create;
{$ENDIF}
  New(FElemList, Init);
  FCloned := false;

  FParser := XML_ParserCreateNS('UTF-8', #0);
  XML_SetElementHandler(FParser, ElementStarted, ElementEnded);
  XML_SetCharacterDataHandler(FParser, CharactersReceived);
  XML_SetUserData(FParser, Self);
end;

procedure TXmlFeed.ParseLine(Line: String; var Item: TFeedItem; var ItemFinished: Boolean);
begin
  inherited ParseLine(Line, Item, ItemFinished);

  XML_Parse(FParser, {$IFDEF __GPC__}Line{$ELSE}PChar(Line){$ENDIF}, Length(Line), 0);
  ItemFinished := true;
end;

{ Clones an element.  This is necessary for namespace support. }
procedure TXmlFeed.Clone(const List: PRList);
begin
  Dispose(FElemList);
  Move(List, FElemList, SizeOf(List));
  FCloned := true;
end;

destructor TXmlFeed.Destroy;
var
  i: cardinal;
  p: PXmlElement;
begin
  Xml_ParserFree(FParser);

  if not FCloned then
  begin
    if FElemList^.Count > 0 then
    begin
      for i := 0 to FElemList^.Count - 1 do
      begin
        p := FElemList^.GetNth(i);
        Dispose(p);
      end;
    end;

    Dispose(FElemList, Done);
  end;

{$IFNDEF __GPC__}
  inherited Destroy;
{$ENDIF}
end;

function TXmlFeed.GetCurrentElement: TXmlElement;
var
  Elem: TXmlElement;
begin
  if FElemList^.Count > 0 then
  begin
    Elem := PXmlElement(FElemList^.GetNth(FElemList^.Count - 1))^;
    if Elem.Base <> '' then
    begin
      FLastBase := Elem.Base;
    end
    else if GetPreviousElement.Base <> '' then
    begin
      Elem.Base := GetPreviousElement.Base;
      FLastBase := Elem.Base;
    end
    else
    begin
      Elem.Base := FLastBase;
    end;
  
    if Elem.Lang <> '' then
    begin
      FLastLang := Elem.Lang;
    end
    else if GetPreviousElement.Lang <> '' then
    begin
      Elem.Lang := GetPreviousElement.Lang;
      FLastLang := Elem.Lang;
    end
    else
    begin
      Elem.Lang := FLastLang;
    end;
  end;

  GetCurrentElement := Elem;
end;

function TXmlFeed.GetPreviousElement: TXmlElement;
begin
  if FElemList^.Count > 1 then
  begin
    GetPreviousElement := PXmlElement(FElemList^.GetNth(FElemList^.Count - 2))^;
  end;
end;

procedure TXmlFeed.StripNS(var Element: String; const NS: String);
begin
  if Pos(NS, Element) = 1 then
  begin
    Delete(Element, 1, Length(NS));
  end;
end;

end.
