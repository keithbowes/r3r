unit Rss;

interface

uses
  Feed, FeedItem, Xml;

const
  RDFNS = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#';
  RSS1NS = 'http://purl.org/rss/1.0/';
  RSS2NS = '';

type
  TRssVersion = (rvRss0, rvRss1, rvRss2);
  TRssFeed = class(TXmlFeed)
  private
    FLastCat: String;
    FLeftChannel: Boolean;
    FRssVersion: TRssVersion;
    procedure HandleNameSpace(const Elem: TXmlElement; Line: String; Item: TFeedItem);
  protected
    function GetFormat: TFeedType; override;
  public
    constructor Create;
    procedure ParseLine(Line: String; var Item: TFeedItem); override;
    procedure SendItem; override;
  end;

implementation

uses
  Atom, DC, Mod_Enclosure, RDate,
  RStrings, SockConsts, SysUtils;

constructor TRssFeed.Create;
begin
  inherited Create;
  FRssVersion := rvRss1;
end;

procedure TRssFeed.ParseLine(Line: String; var Item: TFeedItem);
var
  Elem, Parent: TXmlElement;
begin
  inherited ParseLine(Line, Item);
  Elem := GetCurrentElement;
  Parent := GetParentElement;
  HandleNameSpace(Elem, Line, Item);

  if FRssVersion <> rvRss1 then
  begin
    Item.Finished := ((Elem.Name = 'item') and ((Parent.Name = 'item')
      or FLeftChannel)) or (Line = SockEOF);
  end
  else
  begin
    Item.Finished := (Elem.Name = 'item') or (Line = SockEOF);
  end;

  if Item.Finished and ((Elem.NameSpace = RSS1NS) or (Elem.NameSpace = RSS2NS)) then
  begin
    inherited SendItem; 
    FLeftChannel := false;
  end;
end;

function TRssFeed.GetFormat: TFeedType;
begin
  GetFormat := ftRss;
end;

procedure TRssFeed.SendItem;
var
  Attr: TXmlAttr;
  Elem: TXmlElement;
  Idx: PtrUInt;
begin
  Elem := GetCurrentElement;
  HandleNameSpace(Elem, '', FCurrentItem);
  if ((Elem.NameSpace <> RSS1NS) and (Elem.NameSpace <> RSS2NS) or
    (Elem.Name = '')) then
  begin
    Exit;
  end;

  with Elem, FCurrentItem do
  begin 
    if (Name = 'title') and (GetParentElement.Name <> 'image') then
    begin
      Title := Content;
    end
    else if Name = 'description' then
    begin
      Description := Content;
    end
    else if (Name = 'link') and (GetParentElement.Name <> 'image') then
    begin
      Link := GetAbsoluteURL(Trim(Content));
      if (FRssVersion = rvRss0) and (Id = '') and (Link <> ':///') and (GetParentElement.Name <> 'channel') then
      begin
        Id := Link
      end
    end
    else if Name = 'enclosure' then
    begin
      if Attributes^.Count > 0 then
      begin
        for Idx := 0 to Attributes^.Count - 1 do
        begin
          Attr := PXmlAttr(Attributes^.GetNth(Idx))^;
          if Attr.Name = 'url' then
          begin
            Enclosure.URL := GetAbsoluteURL(Attr.Value);
          end;
        end;

        if Attr.Name = 'type' then
        begin
          Enclosure.MimeType := Attr.Value;
        end;
      end;
    end
    else if Name = 'category' then
    begin
      Content := StringReplace(Content, '/', ', ', [rfReplaceAll]);

      if FLastCat <> Content then
      begin
        if Subject = '' then
        begin
          Subject := Content
        end
        else if Content <> '' then
        begin
          Subject := Content + ', ' + Subject;
        end;

        FLastCat := Content
      end;
    end
    else if Name = 'pubdate' then
    begin
      Created := TimeToLongDate(LongDateToTime(Content));
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
      LastModified := TimeToLongDate(LongDateToTime(Content));
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
    end
    else if Name = 'channel' then
    begin
      if Attributes^.Count > 0 then
      begin
        for Idx := 0 to Attributes^.Count - 1 do
        begin
          Attr := PXMLAttr(Attributes^.GetNth(Idx))^;
          if (Attr.Name = 'about') and (Attr.NameSpace = RDFNS) then
          begin
            MySelf := Attr.Value;
          end;
        end;
      end;

      FLeftChannel := true;
    end
    else if Name = 'rss' then
    begin
      if Attributes^.Count > 0 then
      begin
        for Idx := 0 to Attributes^.Count - 1 do
        begin
          Attr := PXmlAttr(Attributes^.GetNth(Idx))^;
          if Attr.Name = 'version' then
          begin
            if Pos('0.9', Attr.Value) = 1 then
            begin
              FRssVersion := rvRss0;
            end
            else if Pos('2.', Attr.Value) = 1 then
            begin
              FRssVersion := rvRss2;
            end
          end;
        end;
      end;
    end;

    if Name = 'item' then
    begin
      inherited SendItem;

      if (Id = '') and (Attributes^.Count > 0) then
      begin
        for Idx := 0 to Attributes^.Count - 1 do
        begin
          Attr := PXMLAttr(Attributes^.GetNth(Idx))^;
          if (Attr.Name = 'about') and (Attr.NameSpace = RDFNS) then
          begin
            Id := Attr.Value;
          end;
        end;
      end;

      if Uri = '' then
      begin
        Uri := Id;
      end;
    end;
  end;
end;

procedure TRssFeed.HandleNameSpace(const Elem: TXmlElement; Line: String; Item: TFeedItem);
var
  AFeed: TXmlFeed;
begin
  if Elem.NameSpace = DCNS then
  begin
    ParseForeignFeed(Line, Item, TDCFeed);
  end
  else if Elem.NameSpace = AtomNS then
  begin
    ParseForeignFeed(Line, Item, TAtomFeed);
  end
  else if Elem.NameSpace = Mod_EnclosureNS then
  begin
    ParseForeignFeed(Line, Item, TModEnclosure);
  end
end;

end.
