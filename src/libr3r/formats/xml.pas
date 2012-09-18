unit Xml;

interface

uses
{$IFDEF USE_EXPAT}
  Expas,
{$ENDIF}
  Feed, FeedItem, RList;

const
{$IFDEF USE_EXPAT}
  NameSpaceSeparator = XML_NsSeparator;
{$ENDIF}
  XMLNSNS = 'http://www.w3.org/XML/1998/namespace';

type
  PXmlAttr = ^TXmlAttr;
  TXmlAttr = record
    Name: String;
    NameSpace: String;
    Value: String;
  end;

  PXmlElement = ^TXmlELement;
  TXmlElement = record
    Name: String;
    NameSpace: String;
    Attributes: PRList;
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
    FEncoding: PChar;
    FItemFinished: Boolean;
{$IFDEF USE_EXPAT}
    FParser: XML_PARSER;
{$ENDIF}
    constructor Create; {$IFDEF __GPC__}override;{$ENDIF}
    procedure ParseLine(Line: String; var Item: TFeedItem); override;
    destructor Destroy; override;
    procedure Clone(const List: PRList);
    function GetCurrentElement: TXmlElement; virtual;
    function GetPreviousElement: TXmlElement; virtual;
    procedure SendItem; virtual;
  end;

implementation

uses
{$IFDEF USE_EXPAT}
  SaxCallbacks,
{$ENDIF}
   LibR3RStrings, RSettings, RStrings, SysUtils;

constructor TXmlFeed.Create;
{$IFDEF USE_EXPAT}
var
  Encoding: PChar;
{$ENDIF}
begin
{$IFNDEF __GPC__}
  inherited Create;
{$ENDIF}
  New(FElemList, Init);
  Depth := 0;
  FCloned := false;
  FNthElem := 0;

{$IFDEF USE_EXPAT}
  Encoding := {$IFDEF USE_ICONV}'UTF-8'{$ELSE}nil{$ENDIF};
{$IFDEF EXPAT_1_1}
  FParser := XML_ParserCreateNS(Encoding, NameSpaceSeparator);
{$ELSE}
  FParser := XML_ParserCreate(Encoding);
{$ENDIF}

  XML_SetElementHandler(FParser, ElementStarted, ElementEnded);
  XML_SetCharacterDataHandler(FParser, CharactersReceived);

{$IFDEF EXPAT_2_0}
  XML_SetXmlDeclHandler(parser, XmlDeclarationReceived);
{$ELSE}
{$IFDEF EXPAT_1_0}
  XML_SetUnknownEncodingHandler(parser, UnknownEncodingDetected, Self);
{$ENDIF}
{$ENDIF}
  XML_SetUserData(FParser, Self);
{$ENDIF}
end;

procedure TXmlFeed.ParseLine(Line: String; var Item: TFeedItem);
{$IFDEF USE_EXPAT}
var
  pLine: PChar;
{$ENDIF}
begin
  inherited ParseLine(Line, Item);
  CurrentItem := Item;

{$IFDEF USE_EXPAT}
  pLine := DataToUTF8({$IFDEF __GPC__}Line{$ELSE}PChar(Line){$ENDIF}, FEncoding);
  XML_Parse(FParser, pLine, StrLen(pLine), 0);
{$ENDIF}

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
  i, j: PtrUInt;
  p: PXmlElement;
  q: PXmlAttr;
begin
{$IFDEF USE_EXPAT}
  XML_Parse(FParser, '', 0, 1);
  Xml_ParserFree(FParser);
{$ENDIF}

  if not FCloned then
  begin
    if FElemList^.Count > 0 then
    begin
      for i := 0 to FElemList^.Count - 1 do
      begin
        p := FElemList^.GetNth(i);
        if p^.Attributes^.Count > 0 then
        begin
          for j := 0 to p^.Attributes^.Count - 1 do
          begin
            q := p^.Attributes^.GetNth(j);
            Dispose(q);
          end;
        end;
        Dispose(p^.Attributes, Done);
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
begin
  if FElemList^.Count > 0 then
  begin
    FNthElem := FElemList^.Count;
  end;

  if FNthElem > 0 then
  begin
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

procedure TXmlFeed.SendItem;
begin
end;

end.
