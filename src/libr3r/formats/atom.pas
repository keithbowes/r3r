unit Atom;

interface

uses
  Feed, FeedItem, Xml;

type
  TAtomFeed = class(TXmlFeed)
  private
    { Category Type }
    FCatType: String;
    function GetAbsoluteURL(const URL: String): String;
  protected
    function GetFormat: TFeedType; override;
    procedure FillItem(var Item: TFeedItem);
  public
    constructor Create;
    procedure ParseLine(Line: String; var Item: TFeedItem; var ItemFinished: Boolean); override;
  end;

implementation

uses
  DC, RStrings, SockConsts, SynaUtil, SysUtils;

function TAtomFeed.GetAbsoluteURL(const URL: String): String;
var
  Prot, User, Pass, Host, Port, Path, Para: String;
  BaseProt, BaseUser, BasePass, BaseHost, BasePort, BasePath, BasePara: String;
begin
  ParseURL(URL, Prot, User, Pass, Host, Port, Path, Para);

  { URL is absolute if it contains the host name;
    it isn't if it doesn't }
  if Pos(Host, URL) <> 0 then
  begin
    Result := URL;
  end
  else
  begin
    ParseURL(FXmlElement.Base, BaseProt, BaseUser, BasePass, BaseHost, BasePort, BasePath, BasePara);
    Result := BaseProt + '://';

    if BaseUser <> '' then
    begin
      Result := Result + BaseUser;
      if BasePass <> '' then
      begin
        Result := Result + ':' + BasePass;
      end;

      Result := Result + '@';
    end;

    Result := Result + BaseHost;
    
    if Port <> BasePort then
    begin
      Result := Result + ':' + BasePort;
    end;

    Result := Result + Path;

    if Para <> '' then
    begin
      Result := Result + '?' + Para;
    end;
  end;
end;

function TAtomFeed.GetFormat: TFeedType;
begin
  Result := ftAtom;
end;

procedure TAtomFeed.FillItem(var Item: TFeedItem);
var
  DT: TDateTime;
  Idx: integer;
  Link: String;
  PLink: PChar;
begin
  ShortDateFormat := 'YYYY-MM-DDThh:nn:ssZ';

  with Item, FXmlElement do
  begin
    Language := Lang;

    if Name = 'title' then
    begin
      Title := Title + Content;
    end
    else if (Name = 'summary') or (Name = 'subtitle') then
    begin
      Description := Description + Content;
    end
    else if Name = 'link' then
    begin
      for Idx := Low(Attributes) to High(Attributes) do
      begin
        if Attributes[Idx].Name = 'href' then
        begin
          Link := GetAbsoluteURL(Attributes[Idx].Value);
          PLink := StrToPChar(Link);
          Links^.Add(PLink);
        end;
      end;
    end
    else if Name = 'category' then
    begin
      for Idx := Low(Attributes) to High(Attributes) do
      begin
        { The human-readable label attribute should override term }
        if Attributes[Idx].Name = 'label' then
        begin
          if FCatType = 'term' then
          begin
            Subject := '';
          end;

          if Subject <> '' then
          begin
            Subject := Subject + ', ';
          end;

          Subject := Subject + Attributes[Idx].Value;
          FCatType := 'label';
        end
        else if Attributes[Idx].Name = 'term' then
        begin
          if FCatType <> 'label' then
          begin
            if Subject <> '' then
            begin
              Subject := Subject + ', ';
            end;

            Subject := Subject + Attributes[Idx].Value;
            FCatType := 'term';
          end;
        end;
      end;
    end
    else if Name = 'published' then
    begin
    Created := Created + Content;
    try
      DT := StrToDateTime(Created);
    except
    end;
    Created := FormatDateTime('dddd DD MMMM YYYY hh:nn', DT);
    end
    else if Name = 'name' then
    begin
      if PreviousElement.Name = 'email' then
      begin
        Contact^.Toee := Contact^.Toee + Content;
      end;
    end
    else if Name = 'uri' then
    begin
      if PreviousElement.Name = 'email' then
      begin
        Contact^.Address := Contact^.Address + Content;
      end;
    end
    else if Name = 'generator' then
    begin
      Generator := Generator + Content;
    end
    else if Name = 'updated' then
    begin
      LastModified := LastModified + Content;
      try
        DT := StrToDateTime(LastModified);
      except
      end;
      LastModified := FormatDateTime('dddd DD MMMM YYYY hh:nn', DT);
    end
    else if Name = 'rights' then
    begin
      Copyright := Copyright + Content;
    end
    else if Name = 'id' then
    begin
      Id := Id + Content;
      Uri := Id;

      if PreviousElement.Name <> 'entry' then
      begin
        Id := '';
      end;
    end
    else
    begin
      DCFill(Item, Name, Content);
    end;
  end;
end;

constructor TAtomFeed.Create;
begin
  inherited Create;
  FCatType := '';
end;

procedure TAtomFeed.ParseLine(Line: String; var Item: TFeedItem; var ItemFinished: Boolean);
begin
  inherited ParseLine(Line, Item, ItemFinished);
  FillItem(Item);

  ItemFinished := (FXmlElement.Name = 'entry') or (Line = SockEof);
end;

end.
