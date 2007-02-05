unit Esf;

interface

uses
  Classes, Feed, FeedItem;

type
  TLineType = (ltNone, ltData, ltMeta);

  TEsfFeed = class(TFeed)
  private
    FLCount: cardinal;
    FLineType: TLineType;
    FList: TStringList;
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
  DateUtils, SockConsts, SysUtils;

constructor TEsfFeed.Create;
begin
  inherited Create;

  FLineType := ltNone;
  FList := TStringList.Create;
  FRegExpr.Expression := '\t';
end;

destructor TEsfFeed.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

function TEsfFeed.UnixToDate(const TS: String): String;
var
  NTS: integer;
begin
  if TryStrToInt(TS, NTS) then
  begin
    try
      Result := DateTimeToStr(UnixToDateTime(NTS));
    except
      { UnixToDateTime is unimplemented in FPC.  Use this crude surrogate until
        it is. }
      Result := DateTimeToStr((double(NTS) / (24 * 60 * 60)) + EncodeDate(1970, 1, 1));
    end;
  end
  else
  begin
    Result := '';
  end;
end;

function TEsfFeed.GetFormat: TFeedType;
begin
  Result := ftEsf;
end;

procedure TEsfFeed.ParseDataLine(var Item: TFeedItem);
begin
  with Item do
  begin
    Created := UnixToDate(FList[0]);
    Title := Trim(FList[1]);
    LinksCount := Length(Links);
    SetLength(Links, LinksCount + 1);
    Links[LinksCount] := Trim(FList[2]);
  end;
end;

procedure TEsfFeed.ParseMetaLine(var Item: TFeedItem);
begin
  FList[0] := LowerCase(FList[0]);
  FList[1] := Trim(FList[1]);

  with Item do
  begin
    if FList[0] = 'title' then
    begin
      Title := FList[1];
    end
    else if FList[0] = 'link' then
    begin
      LinksCount := Length(Links);
      SetLength(Links, LinksCount + 1);
      Links[LinksCount] := FList[1];
    end
    else if FList[0] = 'contact' then
    begin
      Contact := CreateEmailRecord(FList[1], '(', 1);
    end;
  end;
end;

procedure TEsfFeed.ParseLine(Line: String; var Item: TFeedItem; var ItemFinished: Boolean);
begin
  ItemFinished := true;

  if Pos('#', Line) <> 1 then
  begin
    FList.Clear;
    FRegExpr.Split(Line, FList);
    FLCount := FList.Count;

    if FLCount > 2 then
    begin
      ParseDataLine(Item);
      FLineType := ltData;
      ItemFinished := true;
    end
    else if FLCount = 2 then
    begin
      ParseMetaLine(Item);
      ItemFinished := Trim(Line) <> '';
      FLineType := ltMeta;
    end
    else if Line = SockEof then
    begin
      ItemFinished := true;
    end;
  end;
end;

end.
