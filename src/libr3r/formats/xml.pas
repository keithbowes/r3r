unit Xml;

interface

uses
  Expas, Feed, FeedItem, RList;

const
  AtomNS = 'http://www.w3.org/2005/Atom';
  DCNS = 'http://purl.org/dc/elements/1.1/';
  Mod_EnclosureNS = 'http://purl.oclc.org/net/rss_2.0/enc#';
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
    Depth: cardinal;
  end;

  TXmlFeed = class(TFeed)
  private
    FCloned: Boolean;
    FNthElem: cardinal;
    FLastBase: String;
    FLastLang: String;
  public
    CurrentItem: TFeedItem;
    Depth: cardinal;
    FElemList: PRList;
    FElems: cardinal;
    FParser: XML_PARSER;
    constructor Create; {$IFDEF __GPC__}override;{$ENDIF}
    procedure ParseLine(Line: String; var Item: TFeedItem); override;
    destructor Destroy; override;
    procedure StripNS(var Element: String; const NS: String);
    procedure Clone(const List: PRList);
    function GetCurrentElement: TXmlElement; virtual;
    function GetPreviousElement: TXmlElement; virtual;
    procedure SendItem(const Name, Content: String); virtual;
  end;

implementation

uses
  LibR3RStrings, RMessage, RSettings, RStrings, SaxCallbacks, SysUtils;

constructor TXmlFeed.Create;
begin
{$IFNDEF __GPC__}
  inherited Create;
{$ENDIF}
  New(FElemList, Init);
  Depth := 0;
  FCloned := false;
  FNthElem := 0;

  FParser := XML_ParserCreateNS(nil, #0);
  XML_SetElementHandler(FParser, ElementStarted, ElementEnded);
  XML_SetCharacterDataHandler(FParser, CharactersReceived);
  XML_SetUserData(FParser, Self);
end;

procedure TXmlFeed.ParseLine(Line: String; var Item: TFeedItem);
begin
  inherited ParseLine(Line, Item);

  CurrentItem := Item;
  XML_Parse(FParser, {$IFDEF __GPC__}Line{$ELSE}PChar(Line){$ENDIF}, Length(Line), 0);

  Item.Finished := true;
end;

{ Clones an element.  This is necessary for namespace support. }
procedure TXmlFeed.Clone(const List: PRList);
begin
  Dispose(FElemList, Done);
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

      Dispose(FElemList, Done);
    end;
  end;

{$IFNDEF __GPC__}
  inherited Destroy;
{$ENDIF}
end;

function TXmlFeed.GetCurrentElement: TXmlElement;
var
  Elem: TXmlElement;
  i: cardinal;
  s: String;
const
  LastElemNum: cardinal = 0;
begin
  if FElemList^.Count > 0 then
  begin
    FNthElem := FElemList^.Count;
  end;

  if FNthElem > 0 then
  begin
    if Settings.GetBoolean(Settings.IndexOf('warn-missing-data')) and
      (FNthElem > LastElemNum + 1) then
    begin
      s := '';
      for i := LastElemNum to FNthElem - 1 do
      begin
        s := s + PXmlElement(FElemList^.GetNth(i))^.Content + #13#10;
      end;

      s := Trim(s);
      if Length(s) > 0 then
      begin
        CallMessageEventEx(Self, true, MissingData, s);
      end;
    end;

    Elem := PXmlElement(FElemList^.GetNth(FNthElem))^;
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

  LastElemNum := FNthElem;
end;

function TXmlFeed.GetPreviousElement: TXmlElement;
var
  i: cardinal;
  Res: TXmlElement;
begin
  if FElemList^.Count > 0 then
  begin
    i := 0;
    repeat
      Res := PXmlElement(FElemList^.GetNth(FElemList^.Count - i))^;
      Inc(i);
    until (Res.Depth = Depth - 1) or (i - 1 = FElemList^.Count);
  end;

  GetPreviousElement := Res;
end;

procedure TXmlFeed.StripNS(var Element: String; const NS: String);
begin
  if Pos(NS, Element) = 1 then
  begin
    Delete(Element, 1, Length(NS));
  end;
end;

procedure TXmlFeed.SendItem(const Name, Content: String);
begin
end;

end.
