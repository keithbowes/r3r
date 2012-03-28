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
    function GetAbsoluteURL(const URL: String): String;
  protected
    function GetFormat: TFeedType; override;
  public
    constructor Create; {$IFDEF __GPC__}override;{$ENDIF}
    procedure ParseLine(Line: String; var Item: TFeedItem); override;
    procedure SendItem; override;
    function GetCurrentElement: TXmlElement; override;
    function GetPreviousElement: TXmlElement; override;
  end;

implementation

{$IFDEF SOCKETS_CURL}
{$DEFINE SOCKETS_NONE}
{$ENDIF}

uses
  DC, HttpCache, ItemCallbacks, RDate, RStrings, SockConsts

{$IFDEF __GPC__}
  , SysUtils
{$ENDIF}

{$IFDEF SOCKETS_SYNAPSE}  
  , SynaUtil
{$ENDIF}

{$IFDEF SOCKETS_BSD}
, SockWrap
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
var
  AFeed: TFeed;
begin
  inherited ParseLine(Line, Item);

  if Pos(DCNS, GetCurrentElement.Name) <> 0 then
  begin
    AFeed := TDCFeed.Create;
    (AFeed as TXmlFeed).Clone(FElemList);
    AFeed.ParseLine(Line, Item);
    (AFeed as TDCFeed).Free;
  end;

  Item.Finished := Line = SockEof;
  
  if Item.Finished then
  begin
    CallItemCallback(Item);
    FLeftFeed := false;
  end;

end;

procedure TAtomFeed.SendItem;
var
  Idx: integer;
begin
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
      for Idx := Low(Attributes) to High(Attributes) do
      begin
        if Attributes[Idx].Name = 'href' then
        begin
          FAtomLink.Href := GetAbsoluteURL(Attributes[Idx].Value);
        end
        else if Attributes[Idx].Name = 'rel' then
        begin
          FAtomLink.Rel := Attributes[Idx].Value;
        end
        else if Attributes[Idx].Name = 'type' then
        begin
          FAtomLink.MimeType := Attributes[Idx].Value;
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
      for Idx := Low(Attributes) to High(Attributes) do
      begin
        { The human-readable label attribute should override term }
        if Attributes[Idx].Name = 'label' then
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
            Subject := Subject + Attributes[Idx].Value;
          end;

          FCatType := 'label';
        end
        else if Attributes[Idx].Name = 'term' then
        begin
          if FCatType <> 'label' then
          begin
            if (Subject <> '') and (Content <> '') then
            begin
              Subject := Subject + ', ';
            end;

            if '' <> Content then
            begin
              Subject := Subject + Attributes[Idx].Value;
            end;

            FCatType := 'term';
          end;
        end;
      end;
    end
    else if Name = 'published' then
    begin
      Created := Content;
      Created := TimeToString(ShortDateToTime(Created));
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
      LastModified := TimeToString(ShortDateToTime(LastModified));
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

function TAtomFeed.GetCurrentElement: TXmlElement;
var
  Res: TXmlElement;
begin
  Res := inherited GetCurrentElement;
  StripNS(Res.Name, AtomNS);
  StripNS(Res.Name, LowerCase(AtomNS));

  GetCurrentElement := Res;
end;

function TAtomFeed.GetPreviousElement: TXmlElement;
var
  Res: TXmlElement;
begin
  Res := inherited GetPreviousElement;
  StripNS(Res.Name, AtomNS);
  StripNS(Res.Name, LowerCase(AtomNS));

  GetPreviousElement := Res;
end;

function TAtomFeed.GetFormat: TFeedType;
begin
  GetFormat := ftAtom;
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
