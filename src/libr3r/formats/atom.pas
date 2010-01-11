unit Atom;

interface

uses
  Feed, FeedItem, Xml;

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
    procedure FillItem(var Item: TFeedItem);
    function GetFormat: TFeedType; override;
  public
    constructor Create; {$IFDEF __GPC__}override;{$ENDIF}
    procedure ParseLine(Line: String; var Item: TFeedItem; var ItemFinished: Boolean); override;
    function GetCurrentElement: TXmlElement; override;
    function GetPreviousElement: TXmlElement; override;
  end;

implementation

uses
  DC, RDate, RStrings, SockConsts,

{$IFDEF __GPC__}
  SysUtils,
{$ENDIF}

{$IFDEF SOCKETS_SYNAPSE}  
  SynaUtil
{$ENDIF}

{$IFDEF SOCKETS_BSD}
SockWrap
{$ENDIF};

function TAtomFeed.GetAbsoluteURL(const URL: String): String;
var
  Prot, User, Pass, Host, Port, Path, Para: String;
  BaseProt, BaseUser, BasePass, BaseHost, BasePort, BasePath, BasePara: String;
  Res: String;
begin
  ParseURL(URL, Prot, User, Pass, Host, Port, Path, Para);

  { URL is absolute if it contains the host name;
    it isn't if it doesn't }
  if Pos(Host, URL) <> 0 then
  begin
    Res := URL;
  end
  else
  begin
    ParseURL(GetCurrentElement.Base, BaseProt, BaseUser, BasePass, BaseHost, BasePort, BasePath, BasePara);
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

function TAtomFeed.GetFormat: TFeedType;
begin
  GetFormat := ftAtom;
end;

procedure TAtomFeed.FillItem(var Item: TFeedItem);
var
  Idx: integer;
  PLink: PChar;
begin
  with Item, GetCurrentElement do
  begin
    Language := Lang;

    if Name = 'title' then
    begin
      Title := Title + Content;
    end
    else if (Name = 'subtitle') or (Name = 'summary') then
    begin
      if not FHasLongDesc then
      begin
        Description := Description + Content;
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

      Description := Description + Content;
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
        if Rel = 'alternate' then
        begin
          PLink := StrToPChar(Href);
          Links^.Add(PLink);
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
      Created := TimeToString(ShortDateToTime(Created));
    end
    else if Name = 'name' then
    begin
      if GetPreviousElement.Name = 'email' then
      begin
        Contact^.Toee := Contact^.Toee + Content;
      end;
    end
    else if Name = 'uri' then
    begin
      if GetPreviousElement.Name = 'email' then
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
      LastModified := TimeToString(ShortDateToTime(LastModified));
    end
    else if Name = 'rights' then
    begin
      Copyright := Copyright + Content;
    end
    else if Name = 'id' then
    begin
      Id := Id + Content;
      Uri := Id;

      if FLeftFeed then
      begin
        Id := '';
      end;
    end
    else if Name = 'feed' then
    begin
      FLeftFeed := true;
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

constructor TAtomFeed.Create;
begin
  inherited Create;
  FCatType := '';
  FHasLongDesc := false;
  FHasShortDesc := false;
  FLeftFeed := false;
end;

procedure TAtomFeed.ParseLine(Line: String; var Item: TFeedItem; var ItemFinished: Boolean);
var
  AFeed: TFeed;
begin
  inherited ParseLine(Line, Item, ItemFinished);

  if Pos(DCNS, GetCurrentElement.Name) <> 0 then
  begin
    AFeed := TDCFeed.Create;
    (AFeed as TXmlFeed).Clone(FElemList);
    AFeed.ParseLine(Line, Item, ItemFinished);
    (AFeed as TDCFeed).Free;
  end;

  FillItem(Item);

  ItemFinished := ((GetCurrentElement.Name = 'entry') and ((GetPreviousElement.Name = 'entry') or (FLeftFeed))) or (Line = SockEof);
  
  if ItemFinished and FLeftFeed then
  begin
    FLeftFeed := false;
  end;
end;

end.
