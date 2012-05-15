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
    FLastLine: String;
  protected
    procedure FillItem(var Item: TFeedItem);
    function GetFormat: TFeedType; override;
  public
    constructor Create; {$IFDEF __GPC__}override;{$ENDIF}
    procedure ParseLine(Line: String; var Item: TFeedItem); override;
  end;

implementation

uses
  ItemCallbacks, RDate, RProp, RStrings, SockConsts, SysUtils;

const
  WhiteSpaceChars: set of char = [#0, #8, #9, #10, #13, #32];

constructor TRss3Feed.Create;
begin
  inherited Create;
  FInHead := true;
  FLastLine := '!';
end;

function TRss3Feed.GetFormat: TFeedType;
begin
  GetFormat := ftRss3;
end;

procedure TRss3Feed.FillItem(var Item: TFeedItem);
var
  Data: String;
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
    Item.Link := Data;
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
    Data := Item.Contact.Email + ' ' + Item.Contact.Name + Data;
    Item.Contact := CreateEmailRecord(Data, ' ', 0);
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

procedure TRss3Feed.ParseLine(Line: String; var Item: TFeedItem);
var
  SepPos: word;
begin
  inherited ParseLine(Line, Item);

  if GetProp('shortstring') = nil then
  begin
    FLastLine := Line;
  end;

  Item.Finished := ((Line = '') and (FLastLine = '')) or (Line = SockEof);

  if not Item.Finished then
  begin
    if Line <> '' then
    begin
      if Line[1] in WhiteSpaceChars then
        begin
        while (Length(Line) > 0) and (Line[1] in WhiteSpaceChars) do
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
    end;
  end
  else
  begin
    CallItemCallback(Item);
    ShouldShow := Item.Title <> '';
    FInHead := false;
  end;

  FLastLine := Line;
end;

end.
