unit LocalFile;

interface

uses
  FeedItem, RSock;

type
  TLocalFile = class(TRSock)
  private
    FFileHandle: Text;
  public
    constructor Create(Filename: String);
    destructor Destroy; override;
    procedure Open; override;
    function ParseItem(var Item: TFeedItem): Boolean; override;
    function GetLine: String; override;
  end;

implementation

uses
  Feed, RProp, SockConsts, SysUtils;

constructor TLocalFile.Create(FileName: String);
begin
  FChunkedLength := -1;
  FURI := FileName;
  ShouldShow := true;
end;

destructor TLocalFile.Destroy;
begin
  Close(FFileHandle);
  inherited Destroy;
end;

procedure TLocalFile.Open;
begin
  Assign(FFileHandle, FURI);
  Reset(FFileHandle);
end;

function TLocalFile.GetLine: String;
var
  Res: String;
begin
  if not Eof(FFileHandle) then
  begin
    ReadLn(FFileHandle, Res);
  end
  else
  begin
    Res := SockEof;
  end;

  GetLine := Res;
end;

function TLocalFile.ParseItem(var Item: TFeedItem): Boolean;
var
  FileExt: String;
begin
  FileExt := ExtractFileExt(FURI);

  if FileExt = '.esf' then
  begin
    FeedType := ftEsf;
  end
  else if FileExt = '.r3' then
  begin
    FeedType := ftRss3;
  end
  else if (FileExt = '.rdf') or (FileExt = '.rss') then
  begin
    FeedType := ftRss;
  end
  else if FileExt = '.atom' then
  begin
    FeedType := ftAtom
  end
  else if FileExt = '.xml' then
  begin
    FeedType := ftXml
  end
  else
  begin
    FeedType := ftUnknown;
  end;

  ParseItem := inherited ParseItem(Item);
end;

end.
