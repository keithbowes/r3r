unit Xml;

interface

uses
  Classes, Feed, FeedItem, Sax, Sax_Html;

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
    FInput: TSaxInputSource;
    FReader: THtmlReader;
    FStream: TStringStream;
  protected
    FXmlElement: TXmlElement;
    procedure ElementStarted(Sender: TObject; const NamespaceURI, LocalName, QName: SAXString; Atts: TSAXAttributes);
    procedure ElementEnded(Sender: TObject; const NamespaceURI, LocalName, QName: SAXString);
    procedure CharactersReceived(Sender: TObject; const ch: PSAXChar; AStart, ALength: Integer);
    procedure WhiteSpaceReceived(Sender: TObject; const ch: PSAXChar; AStart, ALength: Integer);
    function GetCurrentElement: TXmlElement;
    function GetPreviousElement: TXmlElement;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ParseLine(Line: String; var Item: TFeedItem; var ItemFinished: Boolean); override;
    property CurrentElement: TXmlElement read GetCurrentElement;
    property PreviousElement: TXmlElement read GetPreviousElement;
  end;

implementation

uses
  SysUtils;

constructor TXmlFeed.Create;
begin
  inherited Create;

  SetLength(FElemList, 1);

  FStream := TStringStream.Create('');
  FIgnoreWhiteSpace := false;
  FInput := TSaxInputSource.Create(FStream);
  FReader := THtmlReader.Create;
  FReader.OnStartElement := @ElementStarted;
  FReader.OnEndElement := @ElementEnded;
  FReader.OnCharacters := @CharactersReceived;
  FReader.OnIgnorableWhitespace := @WhiteSpaceReceived;
end;

procedure TXmlFeed.ParseLine(Line: String; var Item: TFeedItem; var ItemFinished: Boolean);
begin
  FStream.Free;

  FStream := TStringStream.Create(Line);
  FReader.Parse(FInput);

  ItemFinished := true;
end;

destructor TXmlFeed.Destroy;
begin
  FElemList := nil;

  FStream.Free;
  FInput.Free;
  FReader.Free;

  inherited Destroy;
end;

procedure TXmlFeed.ElementStarted(Sender: TObject; const NamespaceURI, LocalName, QName: SAXString; Atts: TSAXAttributes);
var
  Item: integer;
begin
  FXmlElement.Name := LocalName;
  FXmlElement.Content := '';

  if Assigned(Atts) then
  begin
    for Item := 0 to Atts.Length - 1 do
    begin
      if Atts.GetLocalName(Item) = 'xml:base' then
      begin
        FXmlElement.Base := Atts.GetValue(Item);
      end;
      if Atts.GetLocalName(Item) = 'xml:lang' then
      begin
        FXmlElement.Lang := Atts.GetValue(Item);
      end;
      if Item < 11 then
      begin
        FXmlElement.Attributes[Item].Name := Atts.GetLocalName(Item);
        FXmlElement.Attributes[Item].Value := Atts.GetValue(Item);
      end
    end;
  end;

  FElems := Length(FElemList);
  SetLength(FElemList, FElems + 1);
  FElemList[FElems] := FXmlElement;
end;

procedure TXmlFeed.ElementEnded(Sender: TObject; const NamespaceURI, LocalName, QName: SAXString);
begin
  FElems := Length(FElemList);
  if FElems > 0 then
  begin
    SetLength(FElemList, FElems - 1);
  end;

  FXmlElement.Name := LocalName;
end;

procedure TXmlFeed.CharactersReceived(Sender: TObject; const ch: PSAXChar; AStart, ALength: Integer);
begin
  FXmlElement.Content := FXmlElement.Content + ch;

  FIgnoreWhiteSpace := false;
end;

procedure TXmlFeed.WhiteSpaceReceived(Sender: TObject; const ch: PSAXChar; AStart, ALength: Integer);
begin
  if not FIgnoreWhiteSpace then
  begin
    FXmlElement.Content := FXmlElement.Content + ch;
    FIgnoreWhiteSpace := true;
  end;
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

end.
