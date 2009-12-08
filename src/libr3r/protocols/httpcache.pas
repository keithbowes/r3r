unit HttpCache;

interface

uses
	Classes, Feed, Headers;

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
    FCacheInfo: PCacheInfo;
    FIdsList: TStringList;
    FUrl: String;
    procedure InvalidateFile(CFile: String);
    procedure WriteRawData(Data, CacheFile: String);
  public
    constructor Create(const Url: String);
    destructor Destroy; override;
    function GetCacheHeader: String;
    function GetFeedExtension(FeedType: TFeedType): String;
    procedure Invalidate;
    procedure WriteData(const Data: String; DataType: TCacheDataType);
    property IdsList: TStringList read FIdsList;
    property Info: PCacheInfo read FCacheInfo write FCacheInfo;
  end;

var
  CurrentCache: THttpCache;
  FCacheDir: String;

implementation

uses
  RSettings, RSettings_Routines, StrTok, SysUtils;

constructor THttpCache.Create(const Url: String);
var
  Id: String;
  IdsFile: text;
begin
  FIdsList := TStringList.Create;
  FUrl := Url;

  FCacheDir := SettingsDir + PathDelim + 'cache';
  CheckDir(FCacheDir);

  FCacheDir := FCacheDir + PathDelim  + Url;
  CheckDir(FCacheDir);

  ChDir(FCacheDir);

  if FileExists(CacheIdsFile) then
  begin
    Assign(IdsFile, CacheIdsFile);
    Reset(IdsFile);
    while not Eof(IdsFile) do
    begin
      ReadLn(IdsFile, Id);
      FIdsList.Add(Id);
    end;
    Close(IdsFile);
  end;

  New(FCacheInfo);
  CurrentCache := Self;
end;

destructor THttpCache.Destroy;
begin
  FIdsList.Free;
  Dispose(FCacheInfo);
  inherited Destroy
end;

function THttpCache.GetCacheHeader: String;
var
  CacheHeader: String;
  InfoList: TStringsList;
  InfoText: String;
  InfoFile: text;
begin
  ChDir(FCacheDir);
  Assign(InfoFile, CacheInfoFile);
  if FileExists(CacheInfoFile) then
  begin
    Reset(InfoFile);
    ReadLn(InfoFile, InfoText);
  end;

  InfoList := Split(InfoText, Tab);

  try
    case TCacheType(StrToInt(InfoList.Strings[0])) of
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

    Result := CacheHeader + ': ' + InfoList.Strings[1];
  except
    Result := '';
  end;

  try
    Close(InfoFile);
  except
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
      Result := 'atom';
    end;
    ftEsf:
    begin
      Result := 'esf';
    end;
    ftRss:
    begin
      Result := 'rss';
    end;
    ftRss3:
    begin
      Result := 'r3'
    end;
    otherwise
    begin
      Result := 'unknown';
    end;
  end;
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
    Rewrite(RawFile);

  Append(RawFile);

  WriteLn(RawFile, Data);
  Close(RawFile);
end;

end.
