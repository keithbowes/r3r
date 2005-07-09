unit Xml;

interface

uses
  Classes, Feed, FeedItem, Sax, Sax_Html;

type
  TXmlElement = record
    Name: String;
    Content: String;
    Base: String;
    Lang: String;
  end;

  TXmlFeed = class(TFeed)
  private
    FElemList: TList;
    FInput: TSaxInputSource;
    FReader: THtmlReader;
    FStream: TStringStream;
  protected
    FXmlElement: TXmlElement;
    procedure ElementStarted(Sender: TObject; const NamespaceURI, LocalName, QName: SAXString; Atts: TSAXAttributes);
    procedure ElementEnded(Sender: TObject; const NamespaceURI, LocalName, QName: SAXString);
    procedure CharactersReceived(Sender: TObject; const ch: PSAXChar; AStart, ALength: Integer);
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

constructor TXmlFeed.Create;
begin
  inherited Create;

  FElemList := TList.Create;

  FStream := TStringStream.Create('');
  FInput := TSaxInputSource.Create(FStream);
  FReader := THtmlReader.Create;
  FReader.OnStartElement := @ElementStarted;
  FReader.OnEndElement := @ElementEnded;
  FReader.OnCharacters := @CharactersReceived;
  FReader.OnIgnorableWhitespace := @CharactersReceived;
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
  FElemList.Free;

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
    end;
  end;

  FElemList.Add(@FXmlElement);
end;

procedure TXmlFeed.ElementEnded(Sender: TObject; const NamespaceURI, LocalName, QName: SAXString);
begin
  FElemList.Delete(FElemList.Count - 1);
end;

procedure TXmlFeed.CharactersReceived(Sender: TObject; const ch: PSAXChar; AStart, ALength: Integer);
begin
  FXmlElement.Content := FXmlElement.Content + ch;
end;

function TXmlFeed.GetCurrentElement: TXmlElement;
begin
  Result := TXmlElement(FElemList[FElemList.Count - 1]^);
end;

function TXmlFeed.GetPreviousElement: TXmlElement;
begin
  Result := TXmlElement(FElemList[FElemList.Count - 2]^);
end;

end.
