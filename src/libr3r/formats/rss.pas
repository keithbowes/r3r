unit Rss;

interface

uses
  Feed, FeedItem, Xml;

type
  TRssFeed = class(TXmlFeed)
  private
    FLastCat: String;
  protected
    function GetFormat: TFeedType; override;
    procedure FillItem(var Item: TFeedItem);
  public
    procedure ParseLine(Line: String; var Item: TFeedItem; var ItemFinished: Boolean); override;
  end;

implementation

uses
  Atom, DC, RStrings, SockConsts;

procedure TRssFeed.ParseLine(Line: String; var Item: TFeedItem; var ItemFinished: Boolean);
var
  AFeed: TFeed;
  IsRDF: Boolean;
begin
  inherited ParseLine(Line, Item, ItemFinished);

  if Pos(DCNS, FXmlElement.Name) = 1 then
  begin
    AFeed := TDCFeed.Create;
    StripNS(FXmlElement.Name, DCNS);
    (AFeed as TXmlFeed).Clone(FXmlElement);
    AFeed.ParseLine(Line, Item, ItemFinished);
    AFeed.Free;
  end
  else if Pos(AtomNS, FXMLElement.Name) = 1 then
  begin
    AFeed := TAtomFeed.Create;
    StripNS(FXmlElement.Name, AtomNS);
    (AFeed as TXmlFeed).Clone(FXmlElement);
    AFeed.ParseLine(Line, Item, ItemFinished);
    AFeed.Free;
  end
  else
  begin
    IsRDF := Pos(RSS1NS, FXmlElement.Name) <> 0;

    if IsRDF then
    begin
      StripNS(FXmlElement.Name, RSS1NS);
    end;
    
    FillItem(Item);
  end;

  ItemFinished := ((FXmlElement.Name = 'item') and ((PreviousElement.Name = 'item') or IsRDF)) or (Line = SockEof);

  StripNS(FXmlElement.Name, RDFNS);
  ShouldShow := FXmlElement.Name <> 'RDF';
end;

function TRssFeed.GetFormat: TFeedType;
begin
  Result := ftRss;
end;

procedure TRssFeed.FillItem(var Item: TFeedItem);
var
  PLink: PChar;
begin
  with FXmlElement do
  begin
    if (Name = 'title') and (PreviousElement.Name <> 'image') then
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
    end
    else if Name = 'language' then
    begin
      Item.Language := Item.Language + Content;
      FXmlElement.Lang := Item.Language;
    end
    else if Name = 'copyright' then
    begin
      Item.Copyright := Item.Copyright + Content;
    end
    else if Name = 'guid' then
    begin
      Item.Id := Item.Id + Content;
      Item.Uri := Item.Id;
    end;
  end;
end;

end.
