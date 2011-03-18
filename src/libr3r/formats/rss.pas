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
    function GetFormat: TFeedType; override;
  public
    procedure ParseLine(Line: String; var Item: TFeedItem); override;
    function GetCurrentElement: TXmlElement; override;
    procedure SendItem(const Name, Content: String); override;
  end;

implementation

uses
  Atom, DC, Mod_Enclosure, RDate, RStrings, SockConsts, SysUtils;

function GetAtomFeed: TAtomFeed;
begin
  GetAtomFeed := TAtomFeed.Create;
end;

procedure TRssFeed.ParseLine(Line: String; var Item: TFeedItem);
var
  AFeed: TFeed;
  Elem, Prev: TXmlElement;
  IsRDF: Boolean;
begin
  inherited ParseLine(Line, Item);
  Elem := GetCurrentElement;
  Prev := GetPreviousElement;

  if Pos(DCNS, Elem.Name) = 1 then
  begin
    AFeed := TDCFeed.Create;
    (AFeed as TXmlFeed).Clone(FElemList);
    AFeed.ParseLine(Line, Item);
    (AFeed as TXmlFeed).Free;
  end
  else if Pos(LowerCase(AtomNS), Elem.Name) = 1 then
  begin
    AFeed := GetAtomFeed;
    (AFeed as TXmlFeed).Clone(FElemList);
    AFeed.ParseLine(Line, Item);
    (AFeed as TXmlFeed).Free;
  end
  else if Pos(Mod_EnclosureNS, Elem.Name) = 1 then
  begin
    AFeed := TModEnclosure.Create;
    (AFeed as TXmlFeed).Clone(FElemList);
    AFeed.ParseLine(Line, Item);
    (AFeed as TXmlFeed).Free;
  end
  else
  begin
    IsRDF := Pos(RSS1NS, Elem.Name) <> 0;
    if IsRDF then
    begin
      StripNS(Elem.Name, RSS1NS);
    end;
  end;

  if not IsRDF then
  begin
    Item.Finished := ((Elem.Name = 'item') and ((Prev.Name = 'item')
      or FLeftChannel)) or (Line = SockEOF);
  end
  else
  begin
    Item.Finished := (Elem.Name = 'item') or (Line = SockEOF);
  end;

  if Item.Finished and FLeftChannel then
  begin
    FLeftChannel := false;
  end;
end;

function TRssFeed.GetFormat: TFeedType;
begin
  GetFormat := ftRss;
end;

procedure TRssFeed.SendItem(const Name, Content: String);
var
  Idx: byte;
begin
  with GetCurrentElement, CurrentItem do
  begin 
    StripNS(Name, RDFNS);
    StripNS(Name, RSS1NS);

    if (Name = 'title') and (GetPreviousElement.Name <> 'image') then
    begin
      Title := Content;
    end
    else if Name = 'description' then
    begin
      Description := Content;
    end
    else if Name = 'link' then
    begin
      Link := Trim(Content);
    end
    else if Name = 'enclosure' then
    begin
      for Idx := Low(Attributes) to High(Attributes) do
      begin
        if Attributes[Idx].Name = 'url' then
        begin
          Enclosure.URL := Attributes[Idx].Value;
        end;

        if Attributes[Idx].Name = 'type' then
        begin
          Enclosure.MimeType := Attributes[Idx].Value;
        end;
      end;
    end
    else if Name = 'category' then
    begin
      Content := StringReplace(Content, '/', ', ', [rfReplaceAll]);

      if FLastCat <> Content then
      begin
        if Subject <> ''then
        begin
          Subject := Subject + ', ';
        end;

        Subject := Subject + Content;
        FLastCat := Content
      end;
    end
    else if Name = 'pubdate' then
    begin
      Created := Content;
      Created := TimeToString(LongDateToTime(Created));
    end
    else if (Name = 'author') or (Name = 'managingeditor') then
    begin
      Contact := CreateEmailRecord(Content, '(', 1);
    end
    else if Name = 'generator' then
    begin
      Generator := Content;
    end
    else if Name = 'lastpubdate' then
    begin
      LastModified := Content;
      LastModified := TimeToString(LongDateToTime(LastModified));
    end
    else if Name = 'language' then
    begin
      Language := Content;
      Lang := Language;
    end
    else if Name = 'copyright' then
    begin
      Copyright := Content;
    end
    else if Name = 'guid' then
    begin
      Id := Content;
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
