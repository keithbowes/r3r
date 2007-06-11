unit Rss3;

interface

uses
  Feed, FeedItem, NonXml;

type
  TRss3Feed = class(TNonXmlFeed)
  private
    FCurrentField: String;
  protected
    function GetFormat: TFeedType; override;
    procedure FillItem(var Item: TFeedItem);
    procedure AppendFilledItem(var Item: TFeedItem);
  public
    procedure ParseLine(Line: String; var Item: TFeedItem; var ItemFinished: Boolean); override;
  end;

implementation

uses
  SockConsts, SysUtils;

function TRss3Feed.GetFormat: TFeedType;
begin
  Result := ftRss3;
end;

procedure TRss3Feed.FillItem(var Item: TFeedItem);
begin
  FCurrentField := LowerCase(FRegExpr.Match[1]);
  AppendFilledItem(Item);
end;

procedure TRss3Feed.AppendFilledItem(var Item: TFeedItem);
var
  Data: String;
begin
  Data := Trim(FRegExpr.Match[2]) + ' ';

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
    Item.LinksCount := Length(Item.Links);
    SetLength(Item.Links, Item.LinksCount + 1);
    if Item.LinksCount > 0 then
    begin
      Item.Links[0] := Item.Links[0] + Data;
    end
    else
    begin
      Item.Links[Item.LinksCount] := Data;
    end;
  end
  else if FCurrentField = 'subject' then
  begin
    Item.Subject := Item.Subject + Data;
  end
  else if FCurrentField = 'created' then
  begin
    Item.Created := Item.Created + Data;
  end
  else if FCurrentField = 'creator' then
  begin
    Data := Item.Contact.Address + ' ' + Item.Contact.Toee + Data;
    Item.Contact := CreateEmailRecord(Data, ' ', 0);
  end
  else if FCurrentField = 'generator' then
  begin
    Item.Generator := Item.Generator + Data;
  end
  else if FCurrentField = 'last-modified' then
  begin
    Item.LastModified := Item.LastModified + Data;
  end
  else if FCurrentField = 'language' then
  begin
    Item.Language := Item.Language + Data;
  end
  else if FCurrentField = 'rights' then
  begin
    Item.Copyright := Item.Copyright + Data;
  end
  else if FCurrentField = 'guid' then
  begin
    Item.Id := Item.Id + Data;
  end
  else if FCurrentField = 'uri' then
  begin
    Item.Uri := Item.Uri + Data;
  end;
end;

procedure TRss3Feed.ParseLine(Line: String; var Item: TFeedItem; var ItemFinished: Boolean);
begin
  ItemFinished := (Line = '') or (Line = SockEof);

  if not ItemFinished then
  begin
    with FRegExpr do
    begin
      Expression := '^([-_\d\w\.]+):\s*(.*)[\r\n]*';
      Exec(Line);
      if Match[1] <> '' then
      begin
        FillItem(Item);
      end
      else
      begin
        Expression := '^(\s+)(\S*)';
        Exec(Line);
        if Match[1] <> '' then
        begin
          AppendFilledItem(Item);
        end;
      end;
    end;
  end;
end;

end.
