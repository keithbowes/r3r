unit Esf;

interface

uses
  Feed, FeedItem, NonXml;

type
  TLineType = (ltNone, ltData, ltMeta);

  TFieldList = record
    List: array [1..3] of String;
    Count: 0..3;
  end;

  TEsfFeed = class(TNonXmlFeed)
  private
    FLineType: TLineType;
    FList: TFieldList;
    function UnixToDate(const TS: String): String;
  protected
    function GetFormat: TFeedType; override;
    procedure ParseDataLine(var Item: TFeedItem);
    procedure ParseMetaLine(var Item: TFeedItem);
  public
    constructor Create;
    destructor Destroy; override;
    procedure ParseLine(Line: String; var Item: TFeedItem; var ItemFinished: Boolean); override;
  end;

implementation

uses
  SockConsts, RStrings, SysUtils;

constructor TEsfFeed.Create;
begin
  inherited Create;

  FLineType := ltNone;
end;

destructor TEsfFeed.Destroy;
begin
  inherited Destroy;
end;

function TEsfFeed.UnixToDate(const TS: String): String;
const
  SecondsPerDay = 24 * 60 * 60;
var
  DT: TDateTime;
  ErrPos: word;
  NTS: real;
begin
  Val(TS, NTS, ErrPos);
  if ErrPos = 0 then
  begin
    Result := DateTimeToStr(NTS / SecondsPerDay + EncodeDate(1970, 1, 1));
    DT := StrToDateTime(Result);
    Result := FormatDateTime('dddd DD MMMM YYYY hh:nn', DT);
  end
  else
  begin
    Result := ''
  end;
end;

function TEsfFeed.GetFormat: TFeedType;
begin
  Result := ftEsf;
end;

procedure TEsfFeed.ParseDataLine(var Item: TFeedItem);
var
  Link: String;
  PLink: PChar;
begin
  with Item do
  begin
    Created := UnixToDate(FList.List[1]);
    Title := Trim(FList.List[2]);

    Link := FList.List[3];
    PLink := StrToPChar(Link);
    Links^.Add(PLink);

    Id := StrPas(Links^.GetNth(0));
  end;
end;

procedure TEsfFeed.ParseMetaLine(var Item: TFeedItem);
var
  Link: String;
  PLink: PChar;
begin
  FList.List[1] := LowerCase(FList.List[1]);
  FList.List[2] := Trim(FList.List[2]);

  with Item do
  begin
    if FList.List[1] = 'title' then
    begin
      Item.Title := FList.List[2];
    end
    else if FList.List[1] = 'link' then
    begin
      Link := FList.List[2];
      PLink := StrToPChar(Link);
      Links^.Add(PLink);
    end
    else if FList.List[1] = 'contact' then
    begin
      Contact^ := CreateEmailRecord(FList.List[2], '(', 1);
    end;
  end;
end;

procedure TEsfFeed.ParseLine(Line: String; var Item: TFeedItem; var ItemFinished: Boolean);
var
  i: 1..2;
  len: byte;
  tmp: String;
begin
  ItemFinished := false;
  tmp := Line;

  if Pos('#', Line) <> 1 then
  begin
    FList.List[1] := '';
    FList.List[2] := '';
    FList.List[3] := '';
    FList.Count := 0;

    for i := 1 to 2 do
    begin
      len := Pos(#9, tmp);

      if len <> 0 then
      begin
        FList.List[i] := Copy(tmp, 1, len - 1);
        FList.Count := FList.Count + 1;
        Delete(tmp, 1, len);
      end;
    end;

    if Line <> '' then
    begin
      FList.List[FList.Count + 1] := Copy(tmp, 1, Length(tmp));
      FList.Count := FList.Count + 1;
    end;

    if FList.Count > 2 then
    begin
      ParseDataLine(Item);
      FLineType := ltData;
      ItemFinished := true;
    end
    else if FList.Count = 2 then
    begin
      ParseMetaLine(Item);
      ItemFinished := false;
      FLineType := ltMeta;
    end
    else if (Line = SockEof) or ((FLineType = ltMeta) and (Trim(Line) = '')) then
    begin
      ItemFinished := true;
    end;
  end;

  inherited ParseLine(Line, Item, ItemFinished);
end;

end.
