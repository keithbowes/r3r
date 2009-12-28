unit HttpCache;

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
    FIdsList: PRStringList;
    FRootCacheDir: String;
    procedure InvalidateFile(CFile: String);
    procedure WriteRawData(Data, CacheFile: String);
    procedure Clean;
  public
    Info: PCacheInfo;
    constructor Create(const Url: String);
    destructor Destroy; {$IFNDEF __GPC__}override;{$ENDIF}
    function GetCacheHeader: String;
    function GetFeedExtension(FeedType: TFeedType): String;
    procedure Invalidate;
    procedure WriteData(const Data: String; DataType: TCacheDataType);
    function GetIdsList: PRStringList;
  end;

var
  CurrentCache: THttpCache;
  FCacheDir: String;

implementation

uses
{$IFDEF __GPC__}
  GPC,
{$ENDIF}
  RSettings, RSettings_Routines, StrTok;

constructor THttpCache.Create(const Url: String);
{$IFDEF __GPC__}
const
  PathDelim = DirSeparator;
{$ENDIF}
var
  Id: String;
  IdsFile: text;
begin
  New(FIdsList, Init);
  New(Info);

  FRootCacheDir := SettingsDir + PathDelim + 'cache';
  CheckDir(FRootCacheDir);

  FCacheDir := FRootCacheDir + PathDelim  + Url;
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

  Assign(InfoFile, CacheInfoFile);
  if FileExists(CacheInfoFile) then
  begin
    Reset(InfoFile);
    ReadLn(InfoFile, InfoText);
    FileOpen := true;
  end;

  InfoList := Split(InfoText, Tab);

  Val(InfoList.Strings[0], CacheType, ErrPos);
  if ErrPos = 0 then
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
        { TODO }
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
        WriteRawData(Data, CacheFeedFile + '.' + Ext);
      end;
    end;
    cdtIds:
    begin
      WriteRawData(Data, CacheIdsFile)
    end;
    cdtInfo:
    begin
      WriteRawData(Data, CacheInfoFile)
    end;
    cdtResponse:
    begin
      WriteRawData(Data, CacheResponseFile);
    end;
  end;
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

procedure THttpCache.WriteRawData(Data, CacheFile: String);
var
  RawFile: text;
begin
  ChDir(FCacheDir);
  Assign(RawFile, CacheFile);

  if not FileExists(CacheFile) then
  begin
    Rewrite(RawFile);
  end;

  Append(RawFile);

  WriteLn(RawFile, Data);
  Close(RawFile);
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
      CacheFile := Rec.Name + PathDelim + CacheInfoFile;
      if FileExists(CacheFile) then
      begin
        Age := DateTimeToTimeStamp(FileDateToDateTime(FileAge(CacheFile)));
        if Age.Date < Cur.Date - 30 then
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
end;

end.
