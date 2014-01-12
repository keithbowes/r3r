unit Rss;

interface

uses
  Feed, FeedItem, Xml;

const
  RDFNS = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#';
  RSS1NS = 'http://purl.org/rss/1.0/';

type
  TRssFeed = class(TXmlFeed)
  private
    FIsRDF: Boolean;
    FLastCat: String;
    FLeftChannel: Boolean;
    procedure HandleNameSpace(const Elem: TXmlElement; Line: String; Item: TFeedItem);
  protected
    function GetFormat: TFeedType; override;
  public
    procedure ParseLine(Line: String; var Item: TFeedItem); override;
    procedure SendItem; override;
  end;

implementation

uses
  Atom, DC, Mod_Enclosure, RDate,
  RStrings, SockConsts, SysUtils;

function GetAtomFeed: TAtomFeed;
begin
  GetAtomFeed := TAtomFeed.Create;
end;

procedure TRssFeed.ParseLine(Line: String; var Item: TFeedItem);
var
  Elem, Parent: TXmlElement;
begin
  inherited ParseLine(Line, Item);
  Elem := GetCurrentElement;
  Parent := GetParentElement;
  HandleNameSpace(Elem, Line, Item);

  if not FIsRDF then
  begin
    Item.Finished := ((Elem.Name = 'item') and ((Parent.Name = 'item')
      or FLeftChannel)) or (Line = SockEOF);
  end
  else
  begin
    Item.Finished := (Elem.Name = 'item') or (Line = SockEOF);
  end;

  if Item.Finished and not FItemSent then
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
  Idx: PtrUInt;
begin
  if (GetCurrentElement.NameSpace <> RSS1NS) and (GetCurrentElement.NameSpace <> '') then
  begin
    Exit;
  end;

  HandleNameSpace(GetCurrentElement, '', FCurrentItem);
  with GetCurrentElement, FCurrentItem do
  begin 
    if (Name = 'title') and (GetParentElement.Name <> 'image') then
    begin
      Title := Content;
    end
    else if Name = 'description' then
    begin
      Description := Content;
    end
    else if Name = 'link' then
    begin
      Link := GetAbsoluteURL(Trim(Content));
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
    end;

    if (Name = 'item') and not FItemSent then
    begin
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

      inherited SendItem;
    end
    else
    begin
      FItemSent := false;
    end;
  end;
end;

procedure TRssFeed.HandleNameSpace(const Elem: TXmlElement; Line: String; Item: TFeedItem);
var
  AFeed: TXmlFeed;
begin
  if Elem.NameSpace = DCNS then
  begin
    AFeed := TDCFeed.Create;
    AFeed.Clone(FElemList);
    AFeed.ParseLine(Line, Item);
    AFeed.Free;
  end
  else if Elem.NameSpace = AtomNS then
  begin
    AFeed := GetAtomFeed;
    AFeed.Clone(FElemList);
    AFeed.ParseLine(Line, Item);
    AFeed.Free;
  end
  else if Elem.NameSpace = Mod_EnclosureNS then
  begin
    AFeed := TModEnclosure.Create;
    AFeed.Clone(FElemList);
    AFeed.ParseLine(Line, Item);
    AFeed.Free;
  end
  else
  begin
    FIsRDF := Elem.NameSpace = RSS1NS;
  end;
end;

end.
