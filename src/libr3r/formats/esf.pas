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
    constructor Create; {$IFDEF __GPC__}override;{$ENDIF}
    procedure ParseLine(Line: String; var Item: TFeedItem); override;
  end;

implementation

uses
  ItemCallbacks, RDate, RStrings, SysUtils;

constructor TEsfFeed.Create;
begin
  inherited Create;

  FLineType := ltNone;
end;

function TEsfFeed.UnixToDate(const TS: String): String;
const
  SecondsPerDay = 24 * 60 * 60;
var
  ErrPos: word;
  NTS: real;
  Res: String;
begin
  Val(TS, NTS, ErrPos);
  if ErrPos = 0 then
  begin
    Res := RDate.UnixToDate(NTS);
  end
  else
  begin
    Res := ''
  end;

  UnixToDate := Res;
end;

function TEsfFeed.GetFormat: TFeedType;
begin
  GetFormat := ftEsf;
end;

procedure TEsfFeed.ParseDataLine(var Item: TFeedItem);
begin
  with Item do
  begin
    Created := UnixToDate(FList.List[1]);
    Title := Trim(FList.List[2]);
    Link := FList.List[3];
    Id := Link;
  end;
end;

procedure TEsfFeed.ParseMetaLine(var Item: TFeedItem);
begin
  FList.List[1] := LowerCase(FList.List[1]);
  FList.List[2] := Trim(FList.List[2]);

  with Item do
  begin
    if FList.List[1] = 'title' then
    begin
      Title := FList.List[2];
    end
    else if FList.List[1] = 'link' then
    begin
      Link := FList.List[2];
    end
    else if FList.List[1] = 'contact' then
    begin
      Contact := CreateEmailRecord(FList.List[2], '(', 1);
    end;
  end;
end;

procedure TEsfFeed.ParseLine(Line: String; var Item: TFeedItem);
var
  i: 1..2;
  len: byte;
  tmp: String;
begin
  Item.Finished := false;
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
      if FLineType = ltMeta then
      begin
        CallItemCallback(Item);
        Item.Clear;
      end;

      ParseDataLine(Item);
      FLineType := ltData;
      Item.Finished := true;
    end
    else if FList.Count = 2 then
    begin
      ParseMetaLine(Item);
      Item.Finished := false;
      FLineType := ltMeta;
    end
    else if FList.Count = 1 then
    begin
      Item.Finished := true;
    end;
  end;

  if Item.Finished then
  begin
    CallItemCallback(Item);
  end;

  inherited ParseLine(Line, Item);
end;

end.
