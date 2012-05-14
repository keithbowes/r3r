unit LocalFile;

interface

uses
  FeedItem, RSock;

type
  TLocalFile = class(TRSock)
  private
    FFileHandle: Text;
    FFileName: String;
  public
    constructor Create(Filename: String); {$IFDEF __GPC__}override;{$ENDIF}
    destructor Destroy; override;
    procedure Execute; override;
    function ParseItem(var Item: TFeedItem): Boolean; override;
    function GetLine: String; override;
  end;

implementation

uses
  Feed, RProp, SockConsts, SysUtils;

constructor TLocalFile.Create(FileName: String);
begin
  FFileName := FileName;
  ShouldShow := true;
end;

destructor TLocalFile.Destroy;
begin
  Close(FFileHandle);

{$IFNDEF __GPC__}
  inherited Destroy;
{$ENDIF}
end;

procedure TLocalFile.Execute;
begin
  Assign(FFileHandle, FFileName);
  Reset(FFileHandle);
end;

function TLocalFile.GetLine: String;
var
  Res: String{$IFDEF __GPC__}(255){$ENDIF};
begin
  if not Eof(FFileHandle) then
  begin
{$IFNDEF __GPC__}
    ReadLn(FFileHandle, Res);
{$ELSE}
    if not Eoln(FFileHandle) then
    begin
      Read(FFileHandle, Res);
    end
    else
    begin
      ReadLn(FFileHandle, Res);
    end;

    SetProp('shortstring', Self);
{$ENDIF}
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
  FileExt := ExtractFileExt(FFileName);

  if FileExt = '.esf' then
  begin
    FeedType := ftEsf;
  end
  else if FileExt = '.r3' then
  begin
    FeedType := ftRss3;
  end
  else if (FileExt = '.rdf') or (FileExt = '.rss') or (FileExt = '.xml') then
  begin
    FeedType := ftRss;
  end
  else if FileExt = '.atom' then
  begin
    FeedType := ftAtom
  end
  else
  begin
    FeedType := ftUnknown;
  end;

  ParseItem := inherited ParseItem(Item);
end;

end.
