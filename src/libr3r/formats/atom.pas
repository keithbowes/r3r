unit Atom;

interface

uses
  Feed, FeedItem, Xml;

const
  AtomNS = 'http://www.w3.org/2005/Atom';

type
  TAtomLink = record
    Href, MimeType, Rel: String;
  end;

  TAtomFeed = class(TXmlFeed)
  private
    FAtomLink: TAtomLink;
    { Category Type }
    FCatType: String;
    FHasLongDesc: Boolean;
    FHasShortDesc: Boolean;
    FLeftFeed: Boolean;
    procedure HandleNameSpace(const Elem: TXmlElement; Line: String; Item: TFeedItem);
    function GetAbsoluteURL(const URL: String): String;
  protected
    function GetFormat: TFeedType; override;
  public
    constructor Create; {$IFDEF __GPC__}override;{$ENDIF}
    procedure ParseLine(Line: String; var Item: TFeedItem); override;
    procedure SendItem; override;
  end;

implementation

uses
  DC, HttpCache, ItemCallbacks, RDate, RStrings, SockConsts
{$IFDEF __GPC__}
  , SysUtils
{$ENDIF}
{$IFNDEF SOCKETS_NONE}  
  , RParseURL
{$ENDIF};

constructor TAtomFeed.Create;
begin
  inherited Create;
  FCatType := '';
  FHasLongDesc := false;
  FHasShortDesc := false;
  FLeftFeed := false;
end;

procedure TAtomFeed.ParseLine(Line: String; var Item: TFeedItem);
begin
  inherited ParseLine(Line, Item);
  HandleNameSpace(GetCurrentElement, Line, Item);

  Item.Finished := Line = SockEof;
  if Item.Finished then
  begin
    CallItemCallback(Item);
    FLeftFeed := false;
  end;
end;

procedure TAtomFeed.SendItem;
var
  Attr: TXmlAttr;
  Idx: PtrUInt;
begin
  HandleNameSpace(GetCurrentElement, '', CurrentItem);
  with CurrentItem, GetCurrentElement do
  begin
    Language := Lang;

    if Name = 'title' then
    begin
      Title := Content;
    end
    else if (Name = 'subtitle') or (Name = 'summary') then
    begin
      if not FHasLongDesc then
      begin
        Description := Content;
        FHasShortDesc := true;
      end;
    end
    else if Name = 'content' then
    begin
      if FHasShortDesc then
      begin
        FHasShortDesc := false;
        Description := '';
      end;

      Description := Content;
      FHasLongDesc := true;
    end
    else if Name = 'link' then
    begin
      if Attributes^.Count > 0 then
      begin
        for Idx := 0 to Attributes^.Count - 1 do
        begin
          Attr := PXmlAttr(Attributes^.GetNth(Idx))^;
          if Attr.Name = 'href' then
          begin
            FAtomLink.Href := GetAbsoluteURL(Attr.Value);
          end
          else if Attr.Name = 'rel' then
          begin
            FAtomLink.Rel := Attr.Value;
          end
          else if Attr.Name = 'type' then
          begin
            FAtomLink.MimeType := Attr.Value;
          end;
        end;
      end;

      with FAtomLink do
      begin
        if (Rel = 'alternate') or (Rel = '') then
        begin
          Link := Href;
        end
        else if Rel = 'self' then
        begin
          MySelf := Href;
        end
        else if Rel = 'enclosure' then
        begin
          Enclosure.MimeType := MimeType;
          Enclosure.URL := Href;
        end;

        Href := '';
        MimeType := '';
        Rel := '';
      end;
    end
    else if Name = 'category' then
    begin
      for Idx := 0 to Attributes^.Count - 1 do
      begin
        Attr := PXmlAttr(Attributes^.GetNth(Idx))^;
        { The human-readable label attribute should override term }
        if Attr.Name = 'label' then
        begin
          if FCatType = 'term' then
          begin
            Subject := '';
          end;

          if (Subject <> '') and (Content <> '') then
          begin
            Subject := Subject + ', ';
          end;

          if Content <> '' then
          begin
            Subject := Subject + Attr.Value;
          end;

          FCatType := 'label';
        end
        else if Attr.Name = 'term' then
        begin
          if FCatType <> 'label' then
          begin
            if (Subject <> '') and (Content <> '') then
            begin
              Subject := Subject + ', ';
            end;

            if '' <> Content then
            begin
              Subject := Subject + Attr.Value;
            end;

            FCatType := 'term';
          end;
        end;
      end;
    end
    else if Name = 'published' then
    begin
      Created := Content;
      Created := TimeToLongDate(ShortDateToTime(Created));
    end
    else if Name = 'email' then
    begin
      if GetPreviousElement.Name = 'author' then
      begin
        Contact.Email := Content;
      end;
    end
    else if Name = 'name' then
    begin
      if GetPreviousElement.Name = 'author' then
      begin
        Contact.Name := Content;
      end;
    end
    else if Name = 'uri' then
    begin
      if GetPreviousElement.Name = 'author' then
      begin
        Contact.URI := Content;
      end;
    end
    else if Name = 'generator' then
    begin
      Generator := Content;
    end
    else if Name = 'updated' then
    begin
      LastModified := Content;
      LastModified := TimeToLongDate(ShortDateToTime(LastModified));
    end
    else if Name = 'rights' then
    begin
      Copyright := Content;
    end
    else if Name = 'id' then
    begin
      Id := Content;
      Uri := Id;
    end
    else if Name = 'feed' then
    begin
      FLeftFeed := true;
    end;

    if FLeftFeed then
    begin
      Id := '';
    end;

    if (Name = 'entry') then
    begin
      FLeftFeed := false;
      CallItemCallBack(CurrentItem);

      if (Id <> '') and Assigned(CurrentCache) then
      begin
        CurrentCache.WriteData(Id, cdtIds);
      end;
    end;
  end;
end;

function TAtomFeed.GetFormat: TFeedType;
begin
  GetFormat := ftAtom;
end;

procedure TAtomFeed.HandleNameSpace(const Elem: TXmlElement; Line: String; Item: TFeedItem);
var
  AFeed: TXmlFeed;
begin
  if Elem.NameSpace = DCNS then
  begin
    AFeed := TDCFeed.Create;
    AFeed.Clone(FElemList);
    AFeed.ParseLine(Line, Item);
    AFeed.SendItem;
    AFeed.Free;
  end;
end;

function TAtomFeed.GetAbsoluteURL(const URL: String): String;
var
  Prot, User, Pass, Host, Port, Path, Para: String;
  BaseProt, BaseUser, BasePass, BaseHost, BasePort, BasePath, BasePara: String;
  Res: String;
begin
{$IFNDEF SOCKETS_NONE}
  ParseURL(URL, Prot, User, Pass, Host, Port, Path, Para);
{$ENDIF}

  { URL is absolute if it contains the host name;
    it isn't if it doesn't }
  if Pos(Host, URL) <> 0 then
  begin
    Res := URL;
  end
  else
  begin
{$IFNDEF SOCKETS_NONE}
    ParseURL(GetCurrentElement.Base, BaseProt, BaseUser, BasePass, BaseHost, BasePort, BasePath, BasePara);
{$ENDIF}
    Res := BaseProt + '://';

    if BaseUser <> '' then
    begin
      Res := Res + BaseUser;
      if BasePass <> '' then
      begin
        Res := Res + ':' + BasePass;
      end;

      Res := Res + '@';
    end;

    Res := Res + BaseHost;
    
    if Port <> BasePort then
    begin
      Res := Res + ':' + BasePort;
    end;

    Res := Res + Path;

    if Para <> '' then
    begin
      Res := Res + '?' + Para;
    end;
  end;

  GetAbsoluteURL := Res;
end;

end.
