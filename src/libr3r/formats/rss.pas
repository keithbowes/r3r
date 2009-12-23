unit Rss;

interface

uses
  Feed, FeedItem, Xml;

type
  TRssFeed = class(TXmlFeed)
  private
    FLastCat: String;
    FLeftChannel: Boolean;
  protected
    procedure FillItem(var Item: TFeedItem);
    function GetFormat: TFeedType; override;
  public
    procedure ParseLine(Line: String; var Item: TFeedItem; var ItemFinished: Boolean); override;
    function GetCurrentElement: TXmlElement; override;
  end;

implementation

uses
  Atom, DC, RDate, RStrings, SockConsts

{$IFDEF __GPC__}
  , SysUtils
{$ENDIF};

function GetAtomFeed: TAtomFeed;
begin
  GetAtomFeed := TAtomFeed.Create;
end;

procedure TRssFeed.ParseLine(Line: String; var Item: TFeedItem; var ItemFinished: Boolean);
var
  AFeed: TFeed;
  Elem, Prev: TXmlElement;
  IsRDF: Boolean;
begin
  inherited ParseLine(Line, Item, ItemFinished);
  Elem := GetCurrentElement;
  Prev := GetPreviousElement;

  if Pos(DCNS, GetCurrentElement.Name) = 1 then
  begin
    AFeed := TDCFeed.Create;
    (AFeed as TXmlFeed).Clone(FElemList);
    AFeed.ParseLine(Line, Item, ItemFinished);
    (AFeed as TXmlFeed).Free;
  end
  else if Pos(LowerCase(AtomNS), GetCurrentElement.Name) = 1 then
  begin
    AFeed := GetAtomFeed;
    (AFeed as TXmlFeed).Clone(FElemList);
    AFeed.ParseLine(Line, Item, ItemFinished);
    (AFeed as TXmlFeed).Free;
  end
  else
  begin
    Elem := GetCurrentElement;
    IsRDF := Pos(RSS1NS, Elem.Name) <> 0;
    if IsRDF then
    begin
      StripNS(Elem.Name, RSS1NS);
    end;
  end;

  FillItem(Item);

  StripNS(Elem.Name, RSS1NS);
  StripNS(Prev.Name, RSS1NS);

  ItemFinished := ((Elem.Name = 'item') and ((Prev.Name = 'item') or FLeftChannel)) or (Line = SockEof);

  if ItemFinished and FLeftChannel then
  begin
    FLeftChannel :=  false;
  end;

  ShouldShow := ShouldShow and (Elem.Name <> 'RDF');
end;

function TRssFeed.GetFormat: TFeedType;
begin
  GetFormat := ftRss;
end;

procedure TRssFeed.FillItem(var Item: TFeedItem);
var
  PLink: PChar;
begin
  with GetCurrentElement do
  begin
    StripNS(Name, RSS1NS);

    if (Name = 'title') and (GetPreviousElement.Name <> 'image') then
    begin
      Item.Title := Item.Title + Content;
    end
    else if Name = 'description' then
    begin
      Item.Description := Item.Description + Content;
    end
    else if Name = 'link' then
    begin
      PLink := StrToPChar(Content);
      Item.Links^.Add(PLink);
    end
    else if Name = 'category' then
    begin
      if FLastCat <> Content then
      begin
        if Item.Subject <> '' then
        begin
          Item.Subject := Item.Subject + ', ';
        end;
 
        Item.Subject := Item.Subject + Content;
        FLastCat := Content
      end;
    end
    else if Name = 'pubdate' then
    begin
      Item.Created := Item.Created + Content;
      Item.Created := TimeToString(LongDateToTime(Item.Created));
    end
    else if Name = 'managingeditor' then
    begin
      Item.Contact^ := CreateEmailRecord(Content, '(', 1);
    end
    else if Name = 'generator' then
    begin
      Item.Generator := Item.Generator + Content;
    end
    else if Name = 'lastpubdate' then
    begin
      Item.LastModified := Item.LastModified + Content;
      Item.LastModified := TimeToString(LongDateToTime(Item.LastModified));
    end
    else if Name = 'language' then
    begin
      Item.Language := Item.Language + Content;
      Lang := Item.Language;
    end
    else if Name = 'copyright' then
    begin
      Item.Copyright := Item.Copyright + Content;
    end
    else if Name = 'guid' then
    begin
      Item.Id := Item.Id + Content;
      Item.Uri := Item.Id;
    end
    else if Name = 'channel' then
    begin
      FLeftChannel := true;
    end;
  end;
end;

function TRssFeed.GetCurrentElement: TXmlElement;
var
  Res: TXmlElement;
begin
  Res := inherited GetCurrentElement;
  StripNS(Res.Name, RDFNS);

  GetCurrentElement := Res;
end;

end.
