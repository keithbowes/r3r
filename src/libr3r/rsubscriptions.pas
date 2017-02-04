unit RSubscriptions;

interface

uses
  DOM, RList;

type
  PRSubscriptions = ^TRSubscriptions;
  TRSubscriptions = object
  private
    FBodyElem: TDOMNode;
    FDocument: TXMLDocument;
    FElems: TDOMNodeList;
    FFile: String;
    FList: PRList;
  public
    constructor Init;
    destructor Done;
    procedure Add(const url: DOMString; title: DOMString = '');
    function Count: PtrUint;
    procedure Delete(const index: PtrUInt);
    procedure DeleteString(const s: DOMString);
    procedure GetInfo(const index: PtrUInt; out url, name: DOMString);
    function GetNth(const index: PtrUInt): DOMString;
    function IndexOf(const s: DOMString): PtrInt;
  end;

var
  Subscriptions: PRSubscriptions;

implementation

uses
  LibR3RStrings, RSettings_Routines, SysUtils, XMLRead, XMLWrite;

constructor TRSubscriptions.Init;
var
  Elem: TDOMElement;
begin
  New(FList, Init);

  FFile := GetDataDir + 'abos.opml';
  try
    ReadXMLFile(FDocument, FFile);
  except
    try
      FDocument := TXMLDocument.Create;
      Elem := FDocument.CreateElement('opml');
      FDocument.AppendChild(Elem);
      FList^.Add(Elem);

      Elem := FDocument.CreateElement('head');
      FDocument.DocumentElement.AppendChild(Elem);
      FList^.Add(Elem);

      Elem := FDocument.CreateElement('body');
      FDocument.DocumentElement.AppendChild(Elem);
      FList^.Add(Elem);
    except
    end;
  end;
  
  FBodyElem := FDocument.GetElementsByTagName('body')[0];
  FElems := TDOMElement(FBodyElem).GetElementsByTagName('outline');
  WriteLn(FElems.Length);

  if Count = 0 then
  begin
    Add(DOMString(Subscription1));
    Add(DOMString(Subscription2));
    Add(DOMString(Subscription3));
    Add(DOMString(Subscription4));
  end;
end;

destructor TRSubscriptions.Done;
begin
  WriteXMLFile(FDocument, FFile);
  FDocument.Free;
  Dispose(FList, Done);
end;

procedure TRSubscriptions.Add(const url: DOMString; title: DOMString = '');
var
  FElem: TDOMELement;
begin
  if title = '' then
  begin
    title := url;
  end;
  FElem := FDocument.CreateElement('outline');
  FElem.SetAttribute('text', title);
  FElem.SetAttribute('type', 'rss');
  FElem.SetAttribute('xmlUrl', url);
  FBodyElem.AppendChild(FElem);
  FList^.Add(FElem);
end;
  
function TRSubscriptions.Count: PtrUint;
begin
  Result := FElems.Length;
end;

procedure TRSubscriptions.Delete(const index: PtrUInt);
begin
  FBodyElem.RemoveChild(FElems[index]);
end;

procedure TRSubscriptions.DeleteString(const s: DOMString);
begin
  Delete(IndexOf(s));
end;

procedure TRSubscriptions.GetInfo(const index: PtrUInt; out url, name: DOMString);
begin
  with TDOMElement(FElems[index]) do
  begin
    url := GetAttribute('xmlUrl');
    name := GetAttribute('text');
  end;
end;

function TRSubscriptions.GetNth(const index: PtrUInt): DOMString;
var
  name: DOMString;
begin
  GetInfo(index, Result, name);
end;

function TRSubscriptions.IndexOf(const s: DOMString): PtrInt;
var
  i: PtrUInt;
begin
  if Count > 0 then
  for i := 0 to Count - 1 do
  begin
    if TDOMElement(FElems[i]).GetAttribute('xmUrl') = s then
    begin
      Result := i;
      Break;
    end;
  end;
end;

initialization

New(Subscriptions, Init);

finalization

Dispose(Subscriptions, Done);

end.
