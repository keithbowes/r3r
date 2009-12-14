unit Rss3;

interface

uses
  Feed, FeedItem, NonXml;

type
  TRss3Feed = class(TNonXmlFeed)
  private
    FCurrentField: String;
    FData: String;
    FInHead: Boolean;
  protected
    function GetFormat: TFeedType; override;
    procedure FillItem(var Item: TFeedItem);
  public
    constructor Create;
    procedure ParseLine(Line: String; var Item: TFeedItem; var ItemFinished: Boolean); override;
  end;

implementation

uses
  RDate, RStrings, SockConsts, SysUtils;

const
  WhiteSpaceChars: set of char = [#0, #8, #9, #10, #13, #32];

constructor TRss3Feed.Create;
begin
  inherited Create;
  FInHead := true;
end;

function TRss3Feed.GetFormat: TFeedType;
begin
  Result := ftRss3;
end;

procedure TRss3Feed.FillItem(var Item: TFeedItem);
var
  Data: String;
  NumLinks: cardinal;
  PData: PChar;
begin
  Data := FData + ' ';

  if FCurrentField = 'title' then
  begin
    Item.Title := Item.Title + Data;
  end
  else if FCurrentField = 'description' then
  begin
    Item.Description := Item.Description + Data;
  end
  else if FCurrentField = 'link' then
  begin
    Data := TrimRight(Data);
    NumLinks := Item.LinksCount;

    if NumLinks > 0 then
    begin
      PData := StrToPChar(Data);
      Item.Links^.Add(PData);
    end
    else
    begin
      PData := StrToPChar(Data);
      Item.Links^.Add(PData);
    end;
  end
  else if FCurrentField = 'subject' then
  begin
    Item.Subject := Item.Subject + Data;
  end
  else if FCurrentField = 'created' then
  begin
    Item.Created := Item.Created + Data;
    Item.Created := TimeToString(ShortDateToTime(Item.Created));
  end
  else if FCurrentField = 'creator' then
  begin
    Data := Item.Contact^.Address + ' ' + Item.Contact^.Toee + Data;
    Item.Contact^ := CreateEmailRecord(Data, ' ', 0);
  end
  else if FCurrentField = 'generator' then
  begin
    Item.Generator := Item.Generator + Data;
  end
  else if FCurrentField = 'last-modified' then
  begin
    Item.LastModified := Item.LastModified + Data;
    Item.LastModified := TimeToString(ShortDateToTime(Item.LastModified));
  end
  else if FCurrentField = 'language' then
  begin
    Item.Language := Item.Language + Data;
  end
  else if FCurrentField = 'rights' then
  begin
    Item.Copyright := Item.Copyright + Data;
  end
  else if (FCurrentField = 'guid') and not FInHead then
  begin
    Item.Id := Item.Id + Data;
  end
  else if FCurrentField = 'uri' then
  begin
    Item.Uri := Item.Uri + Data;
  end;
end;

procedure TRss3Feed.ParseLine(Line: String; var Item: TFeedItem; var ItemFinished: Boolean);
var
  SepPos: word;
begin
  inherited ParseLine(Line, Item, ItemFinished);
  ItemFinished := (Line = '') or (Line = SockEof);

  if not ItemFinished then
  begin
    if Line[1] in WhiteSpaceChars then
    begin
      while Line[1] in WhiteSpaceChars do
      begin
        Delete(Line, 1, 1);
      end;

      FData := Line;
    end
    else
    begin
      SepPos := Pos(':', Line);
      FCurrentField := LowerCase(Copy(Line, 1, SepPos - 1));
      FData := Copy(Line, SepPos + 2, Length(Line) - SepPos - 1);
    end;

    FillItem(Item);
  end
  else
  begin
    FInHead := false;
  end;
end;

end.
