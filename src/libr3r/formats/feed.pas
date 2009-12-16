unit Feed;

interface

uses
  FeedItem;

type
  TFeedType = (ftUnset, ftXml, ftAtom, ftDc, ftEsf, ftRss, ftRss3, ftUnknown);

  TFeed = class
  private
    FFormat: TFeedType;
  protected
    function GetFormat: TFeedType; {$IFNDEF __GPC__}virtual;{$ENDIF}
  public
    ShouldShow: Boolean;
    constructor Create;
    procedure ParseLine(Line: String; var Item: TFeedItem; var ItemFinished: Boolean); {$IFNDEF __GPC__}virtual;{$ENDIF}
  end; 

implementation

uses
  HttpCache, RSettings, SockConsts;

constructor TFeed.Create;
begin
{$IFNDEF __GPC__}
  inherited Create;
{$ENDIF}

  ShouldShow := true;
end;

procedure TFeed.ParseLine(Line: String; var Item: TFeedItem; var ItemFinished: Boolean);
var
  HideItems: Boolean;
begin
  { Don't cache local files }
  if CurrentCache <> nil then
  begin
    HideItems := Settings.GetBoolean(Settings.IndexOf('hide-cached-feed-items'));
    HideItems := HideItems or Settings.GetBoolean(Settings.IndexOf('display-feed-title-only'));

    with CurrentCache do
    begin
      Info^.HeaderRec.ContentType := GetFormat;

      if Line <> SockEof then
      begin
        WriteData(Line, cdtFeed);
      end;

      if Item.Id <> '' then
      begin
        if HideItems then
        begin
          WriteData(Item.Id, cdtIds);
        end;

        if (GetIdsList^.IndexOf(Item.Id) <> -1) and HideItems then
        begin
          Item.Clear;
          ShouldShow := false;
        end;

        Item.Id := '';
      end;
    end;
  end;
end;

function TFeed.GetFormat: TFeedType;
begin
  GetFormat := ftUnknown;
end;

end.
