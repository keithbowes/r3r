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
  Atom, DC, RDate, RStrings, SockConsts, SysUtils;

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
  Idx: byte;
  PLink: PChar;
begin
  with GetCurrentElement, Item do
  begin
    StripNS(Name, RSS1NS);

    if (Name = 'title') and (GetPreviousElement.Name <> 'image') then
    begin
      Title := Title + Content;
    end
    else if Name = 'description' then
    begin
      Description := Description + Content;
    end
    else if Name = 'link' then
    begin
      PLink := StrToPChar(Content);
      Links^.Add(PLink);
    end
    else if Name = 'enclosure' then
    begin
      for Idx := Low(Attributes) to High(Attributes) do
      begin
        if Attributes[Idx].Name = 'url' then
        begin
          Enclosure := Attributes[Idx].Value;
        end;
      end;
    end
    else if Name = 'category' then
    begin
      if FLastCat <> Content then
      begin
        if Subject <> '' then
        begin
          Subject := Subject + ', ';
        end;
 
        Subject := Subject + Content;
        FLastCat := Content
      end;
    end
    else if Name = 'pubdate' then
    begin
      Created := Created + Content;
      Created := TimeToString(LongDateToTime(Created));
    end
    else if Name = 'managingeditor' then
    begin
      Contact^ := CreateEmailRecord(Content, '(', 1);
    end
    else if Name = 'generator' then
    begin
      Generator := Generator + Content;
    end
    else if Name = 'lastpubdate' then
    begin
      LastModified := LastModified + Content;
      LastModified := TimeToString(LongDateToTime(LastModified));
    end
    else if Name = 'language' then
    begin
      Language := Language + Content;
      Lang := Language;
    end
    else if Name = 'copyright' then
    begin
      Copyright := Copyright + Content;
    end
    else if Name = 'guid' then
    begin
      Id := Id + Content;
      Uri := Id;
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
