unit HttpCache;

{$unitpath ../formats}

interface

uses
  Feed, Headers, RList, SysUtils;

const
  CacheFeedFile = 'feed';
  CacheIdsFile = 'ids';
  CacheInfoFile = 'info';
  CacheResponseFile = 'response';
  Tab = #9;

type
  TCacheDataType = (cdtFeed, cdtIds, cdtInfo, cdtResponse);
  TCacheType = (ctNone, ctEtag, ctExpires, ctLastModified);

  TCacheInfo = record
    CacheParam: String; { the data of the cache type }
    CacheType: TCacheType;
    DateInfo: TDateTime;
    FeedData: String;
    HeaderRec: THeaders;
  end;
  PCacheInfo = ^TCacheInfo;

  THttpCache = class
  private
    FCurrentDir: String;
    FIdsList: PRStringList;
    FRootCacheDir: String;
    procedure InvalidateFile(CFile: String);
    procedure WriteRawData(Data, CacheFile: String; const Overwrite: Boolean);
    procedure Clean;
  public
    Info: PCacheInfo;
    constructor Create(const Url: String);
    destructor Destroy; {$IFNDEF __GPC__}override;{$ENDIF}
    function GetCacheHeader: String;
    function GetEncoding: String;
    function GetFeedExtension(FeedType: TFeedType): String;
    procedure Invalidate;
    procedure WriteData(const Data: String; DataType: TCacheDataType);
    function GetIdsList: PRStringList;
  end;

function CacheEncode(const URL: String): String;

var
  CurrentCache: THttpCache;
  FCacheDir: String;

implementation

uses
{$IFDEF __GPC__}
  GPC,
{$ENDIF}
  RSettings, RSettings_Routines, StrTok;

function CacheEncode(const URL: String): String;
var
  Res: String;
begin
  Res := StringReplace(URL, '/', '_', [rfReplaceAll]);
  Res := StringReplace(Res, '?', '__', [rfReplaceAll]);
  CacheEncode := Res;
end;

constructor THttpCache.Create(const Url: String);
var
  Id: String;
  IdsFile: text;
begin
  New(FIdsList, Init);
  New(Info);

  FCurrentDir := GetCurrentDir;
  FRootCacheDir := CacheDir;
  CheckDir(FRootCacheDir);

  FCacheDir := FRootCacheDir  + Url;
  CheckDir(FCacheDir);

  ChDir(FCacheDir);

  if FileExists(CacheIdsFile) then
  begin
    Assign(IdsFile, CacheIdsFile);
    Reset(IdsFile);
    while not Eof(IdsFile) do
    begin
      ReadLn(IdsFile, Id);
      FIdsList^.Add(Id);
    end;
    Close(IdsFile);
  end;

  CurrentCache := Self;
  ChDir(FCurrentDir);
end;

destructor THttpCache.Destroy;
begin
  Clean;
  Dispose(FIdsList, Done);
  Dispose(Info);

{$IFNDEF __GPC__}
  inherited Destroy;
{$ENDIF}
end;

function THttpCache.GetCacheHeader: String;
var
  CacheHeader: String;
  CacheType, ErrPos: byte;
  FileOpen: Boolean;
  InfoList: TStringsList;
  InfoText: String;
  InfoFile: text;
begin
  FileOpen := false;
  ChDir(FCacheDir);

  Assign(InfoFile, CacheInfoFile);
  if FileExists(CacheInfoFile) then
  begin
    Reset(InfoFile);
    ReadLn(InfoFile, InfoText);
    FileOpen := true;
  end;

  InfoList := Split(InfoText, Tab);

  Val(InfoList.Strings[0], CacheType, ErrPos);
  if (ErrPos = 0) and (TCacheType(CacheType) <> ctNone) then
  begin
    case TCacheType(CacheType) of
      ctEtag:
      begin
        CacheHeader := 'If-None-Match';
      end;
      ctLastModified:
      begin
        CacheHeader := 'If-Modified-Since';
      end;
      otherwise
      begin
        CacheHeader := '';
      end;
    end;


    GetCacheHeader := CacheHeader + ': ' + InfoList.Strings[1];
  end
  else
  begin
    GetCacheHeader := '';
  end;

  if FileOpen then
  begin
    Close(InfoFile);
  end;
  ChDir(FCurrentDir);
end;

procedure THttpCache.Invalidate;
begin
  ChDir(FCacheDir);

  InvalidateFile(CacheFeedFile + '.atom');
  InvalidateFile(CacheFeedFile + '.esf');
  InvalidateFile(CacheFeedFile + '.r3');
  InvalidateFile(CacheFeedFile + '.rss');
  InvalidateFile(CacheIdsFile);
  InvalidateFile(CacheInfoFile);
  InvalidateFile(CacheResponseFile);

  ChDir(FCurrentDir);
end;

procedure THttpCache.WriteData(const Data: String; DataType: TCacheDataType);
var
  Ext: String;
begin
  case DataType of
    cdtFeed:
    begin
      Ext := GetFeedExtension(Info^.HeaderRec.ContentType);
      if Ext <> 'unknown' then
      begin
        WriteRawData(Data, CacheFeedFile + '.' + Ext, false);
      end;
    end;
    cdtIds:
    begin
      WriteRawData(Data, CacheIdsFile, false)
    end;
    cdtInfo:
    begin
      WriteRawData(Data, CacheInfoFile, true)
    end;
    cdtResponse:
    begin
      WriteRawData(Data, CacheResponseFile, false);
    end;
  end;
end;

function THttpCache.GetEncoding: String;
var
  FileOpen: Boolean;
  InfoList: TStringsList;
  InfoText: String;
  InfoFile: text;
begin
  FileOpen := false;
  ChDir(FCacheDir);

  Assign(InfoFile, CacheInfoFile);
  if FileExists(CacheInfoFile) then
  begin
    Reset(InfoFile);
    ReadLn(InfoFile, InfoText);
    FileOpen := true;
  end;

  InfoList := Split(InfoText, Tab);
  if InfoList.Length > 3 then
  begin
    GetEncoding := InfoList.Strings[3];
  end
  else
  begin
    GetEncoding := '';
  end;

  if FileOpen then
  begin
    Close(InfoFile);
  end;
  ChDir(FCurrentDir);
end;

function THttpCache.GetFeedExtension(FeedType: TFeedType): String;
begin
  case FeedType of
    ftAtom:
    begin
      GetFeedExtension := 'atom';
    end;
    ftEsf:
    begin
      GetFeedExtension := 'esf';
    end;
    ftRss:
    begin
      GetFeedExtension := 'rss';
    end;
    ftRss3:
    begin
      GetFeedExtension := 'r3'
    end;
    otherwise
    begin
      GetFeedExtension := 'unknown';
    end;
  end;
end;

function THttpCache.GetIdsList: PRStringList;
begin
  GetIdsList := FIdsList;
end;

procedure THttpCache.InvalidateFile(CFile: String);
var
  TFile: text;
begin
  if FileExists(CFile) then
  begin
    Assign(TFile, CFile);
    Rewrite(TFile);
    Close(TFile)
  end
end;

procedure THttpCache.WriteRawData(Data, CacheFile: String; const Overwrite: Boolean);
var
  RawFile: text;
begin
  ChDir(FCacheDir);
  Assign(RawFile, CacheFile);

  if not FileExists(CacheFile) or Overwrite then
  begin
    Rewrite(RawFile);
  end;

  if not Overwrite then
  begin
    Append(RawFile);
  end;

  WriteLn(RawFile, Data);
  Close(RawFile);
  
  ChDir(FCurrentDir);
end;

procedure THttpCache.Clean;
var
  Age, Cur: TTimeStamp;
  CacheFile: String;
  Rec: TSearchRec;

function IsEmpty(const Path: String): Boolean;
var
  Rec: TSearchRec;
  Search: String;
begin
  Search := Path + PathDelim + '*';
  FindFirst(Search, faSysFile, Rec);
  IsEmpty := (Rec.Name = '') or (Rec.Name = Search);
  FindClose(Rec);
end;

procedure Empty(const Dir: String);
var
  Rec: TSearchRec;
begin
  FindFirst(Dir + PathDelim + '*', faSysFile, Rec);
  repeat
    DeleteFile(Dir + PathDelim + Rec.Name);
  until FindNext(Rec) <> 0;
  FindClose(Rec);
end;

begin
  Cur := DateTimeToTimeStamp(Now);

  ChDir(FRootCacheDir);
  FindFirst('*', faDirectory, Rec);
  repeat
    if not IsEmpty(Rec.Name) then
    begin
      CacheFile := Rec.Name + PathDelim + CacheResponseFile;
      if FileExists(CacheFile) then
      begin
        Age := DateTimeToTimeStamp(FileDateToDateTime(FileAge(CacheFile)));
        if Age.Date < Cur.Date - Settings.GetInteger('cache-expiry') then
        begin
          Empty(Rec.Name);
          RemoveDir(Rec.Name);
        end;
      end;
    end
    else
    begin
      RemoveDir(Rec.Name);
    end;
  until FindNext(Rec) <> 0;
  FindClose(Rec);

  ChDir(FCurrentDir);
end;

end.
