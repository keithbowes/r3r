unit Xml;

interface

uses
{$IFDEF USE_EXPAT}
  Expas,
{$ENDIF}
  Feed, FeedItem, RList;

const
  NameSpaceSeparator = #$FF;
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
    { Yeah, like anybody will exceed 11 attributes per element }
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
begin
{$IFNDEF __GPC__}
  inherited Create;
{$ENDIF}
  New(FElemList, Init);
  Depth := 0;
  FCloned := false;
  FNthElem := 0;

{$IFDEF USE_EXPAT}
  FParser := XML_ParserCreateNS(nil, NameSpaceSeparator);
  XML_SetElementHandler(FParser, ElementStarted, ElementEnded);
  XML_SetCharacterDataHandler(FParser, CharactersReceived);
  XML_SetProcessingInstructionHandler(FParser, InstructionReceived);
  XML_SetUserData(FParser, Self);
{$ENDIF}
end;

procedure TXmlFeed.ParseLine(Line: String; var Item: TFeedItem);
begin
  inherited ParseLine(Line, Item);
  CurrentItem := Item;

{$IFDEF USE_EXPAT}
  XML_Parse(FParser, {$IFDEF __GPC__}Line{$ELSE}PChar(Line){$ENDIF}, Length(Line), 0);
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
