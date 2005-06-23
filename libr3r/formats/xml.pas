unit Xml;

interface

uses
  Classes, Feed, FeedItem, Sax, Sax_Html;

type
  TXmlElement = record
    Base: String;
    Name: String;
    Lang: String;
  end;

  TXmlFeed = class(TFeed)
  private
    FElemList: TList;
    FInput: TSaxInputSource;
    FReader: THtmlReader;
    FStream: TStringStream;
    FXmlElement: TXmlElement;
  protected
    procedure ElementStarted(Sender: TObject; const NamespaceURI, LocalName, QName: SAXString; Atts: TSAXAttributes);
    procedure ElementEnded(Sender: TObject; const NamespaceURI, LocalName, QName: SAXString);
  public
    constructor Create;
    destructor Destroy; override;
    procedure ParseLine(Line: String; var Item: TFeedItem; var ItemFinished: Boolean); override;
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
begin
  FXmlElement.Name := LocalName;
  {if FElemList.Count > 1 then
  begin
    if TXmlElement(FElemList[FElemList.Count-1]^).Lang = '' then
    begin
      TXmlElement(FElemList[FElemList.Count-1]^).Lang := TXmlElement(FElemList[FElemList.Count-2]^).Lang;
    end;
  end;}

  FElemList.Add(@FXmlElement);
end;

procedure TXmlFeed.ElementEnded(Sender: TObject; const NamespaceURI, LocalName, QName: SAXString);
begin
  FElemList.Delete(FElemList.Count-1);
  WriteLn(TXmlElement(FElemList[FElemList.Count-1]^).Name);
end;

end.
