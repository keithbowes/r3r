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
    Depth: PtrUInt;
  end;

  TXmlFeed = class;
  TForeignXmlCLass = class of TXmlFeed;

  TXmlFeed = class(TFeed)
  private
    FForeignFeed: TXmlFeed;
    FLastBase: String;
    FLastLang: String;
{$IFDEF USE_EXPAT}
    FParser: XML_PARSER;
{$ENDIF}
  protected
    FCurrentItem: TFeedItem;
    procedure ParseForeignFeed(const Line: String; var Item: TFeedItem; const FeedClass: TForeignXmlClass);
  public
    FDepth: PtrUInt;
    FElemList: PRList;
    FEncoding: PChar;
    constructor Create;
    procedure ParseLine(Line: String; var Item: TFeedItem); override;
    destructor Destroy; override;
    function GetAbsoluteURL(const URL: String): String;
    function GetCurrentElement: TXmlElement; virtual;
    function GetParentElement: TXmlElement; virtual;
    procedure SendItem; virtual;
  end;

implementation

uses
{$IFDEF USE_EXPAT}
  SaxCallbacks,
{$ENDIF}
   ItemCallbacks, LibR3RStrings, RSettings, RStrings, SysUtils;

constructor TXmlFeed.Create;
{$IFDEF USE_EXPAT}
var
  Encoding: PChar;
{$ENDIF}
begin
  inherited Create;

  New(FElemList, Init);
  FDepth := 0;

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
  XML_SetXmlDeclHandler(FParser, XmlDeclarationReceived);
{$ELSE}
{$IFDEF EXPAT_1_0}
  XML_SetUnknownEncodingHandler(FParser, UnknownEncodingDetected, Self);
{$ENDIF}
{$ENDIF}
  XML_SetUserData(FParser, Self);
{$ENDIF}
end;

procedure TXmlFeed.ParseLine(Line: String; var Item: TFeedItem);
{$IFDEF USE_EXPAT}
var
  Converted: Boolean;
  pLine: PChar;
{$ENDIF}
begin
  inherited ParseLine(Line, Item);
  Item.AllowsHTML := true;
  FCurrentItem := Item;

{$IFDEF USE_EXPAT}
  pLine := DataToUTF8(Line, FEncoding, Converted);
  if StrLen(pLine) > 0 then
  begin
    XML_Parse(FParser, pLine, StrLen(pLine), XML_FALSE);
  end;

  if Converted then
  begin
    FreeMem(pLine);
  end;
{$ENDIF}

  Item.Finished := true;
end;

destructor TXmlFeed.Destroy;
var
  i, j: PtrUInt;
  p: PXmlElement;
  q: PXmlAttr;
begin
{$IFDEF USE_EXPAT}
  XML_Parse(FParser, '', 0, XML_TRUE);
  XML_ParserFree(FParser);
{$ENDIF}

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
  end;

  if Assigned(FForeignFeed) then
  begin
    FForeignFeed.Free;
  end;

  Dispose(FElemList, Done);
  inherited Destroy;
end;

function TXmlFeed.GetAbsoluteURL(const URL: String): String;
begin
  GetAbsoluteURL := inherited GetAbsoluteURL(URL, GetCurrentElement.Base);
end;

function TXmlFeed.GetCurrentElement: TXmlElement;
var
  ElemCount: PtrUInt;
  Elem: TXmlElement;
begin
  ElemCount := FElemList^.Count;
  if ElemCount > 0 then
  begin
    Elem := PXmlElement(FElemList^.GetNth(ElemCount))^;
    if Elem.Base <> '' then
    begin
      FLastBase := Elem.Base;
    end
    else if GetParentElement.Base <> '' then
    begin
      Elem.Base := GetParentElement.Base;
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
    else if GetParentElement.Lang <> '' then
    begin
      Elem.Lang := GetParentElement.Lang;
      FLastLang := Elem.Lang;
    end
    else
    begin
      Elem.Lang := FLastLang;
    end;
  end;

  GetCurrentElement := Elem;
end;

function TXmlFeed.GetParentElement: TXmlElement;
var
  i: PtrUInt;
  Res: TXmlElement;
begin
  if (FElemList^.Count > 0) and (FDepth > 0) then
  begin
    i := 0;
    repeat
      Res := PXmlElement(FElemList^.GetNth(FElemList^.Count - i))^;
      Inc(i);
    until (Res.Depth + 1 = FDepth - 1) or (i - 1 = FElemList^.Count);
  end;

  GetParentElement := Res;
end;

procedure TXmlFeed.SendItem;
begin
  with FCurrentItem do
  begin
    if ShouldShow then
    begin
      CallItemCallBack(FCurrentItem);
    end;
    if FElemList^.Count > 0 then
    begin
      PXmlElement(FElemList^.GetNth(FElemList^.Count))^.Name := '';
    end;
  end;
end;

procedure TXmlFeed.ParseForeignFeed(const Line: String; var Item: TFeedItem; const FeedClass: TForeignXmlClass);
begin
  if not Assigned(FForeignFeed) then
  begin
    FForeignFeed := FeedClass.Create;
  end;

  if not (FForeignFeed is FeedClass) then
  begin
    FForeignFeed.Free;
    FForeignFeed := FeedClass.Create;
  end;

  (FForeignFeed as FeedClass).ParseLine(Line, Item);
end;

end.
