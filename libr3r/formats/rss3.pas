unit Rss3;

interface

uses
  Feed, FeedItem;

type
  TRss3Feed = class(TFeed)
  private
    FCurrentField: String;
  protected
    function GetFormat: TFeedType; override;
    procedure FillItem(var Item: TFeedItem);
    procedure AppendFilledItem(var Item: TFeedItem);
  public
    function ParseLine(Line: String; var Item: TFeedItem): Boolean; override;
  end;

implementation

uses
  RSock, SysUtils;

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
  Data := Trim(FRegExpr.Match[2]);

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
    if Item.Links.Count > 0 then
    begin
      Item.Links.Insert(0, Item.Links[0] + Data);
    end
    else
    begin
      Item.Links.Add(Data);
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
  else if FCurrentField = 'contact' then
  begin
    Item.Contact := Item.Contact + Data
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

function TRss3Feed.ParseLine(Line: String; var Item: TFeedItem): Boolean;
begin
  Result := (Line <> '') and (Line <> SockEof);

  if Result then
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
        Expression := '^\s+(\S*)';
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
