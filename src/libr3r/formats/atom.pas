unit Atom;

interface

uses
  Feed, FeedItem, Xml;

type
  TAtomFeed = class(TXmlFeed)
  private
    { Category Type }
    FCatType: String;
    FHasSelf: Boolean;
    FLeftFeed: Boolean;
    function GetAbsoluteURL(const URL: String): String;
  protected
    function GetFormat: TFeedType; override;
    procedure FillItem(var Item: TFeedItem);
  public
    constructor Create; {$IFDEF __GPC__}override;{$ENDIF}
    procedure ParseLine(Line: String; var Item: TFeedItem; var ItemFinished: Boolean); override;
    function GetCurrentElement: TXmlElement; override;
    function GetPreviousElement: TXmlElement; override;
  end;

implementation

uses
  DC, RDate, RStrings, SockConsts, SysUtils,
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
  Link: String;
  PLink: PChar;
begin
  with Item, GetCurrentElement do
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

          if FHasSelf then
          begin
            Myself := Link;
            FHasSelf := false;
          end;
        end
        else if (Attributes[Idx].Name = 'rel') and (Attributes[Idx].Value = 'self') then
        begin
          FHasSelf := true;
          Myself := GetMainLink;
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

      if GetPreviousElement.Name <> 'entry' then
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
  FHasSelf := false;
  FLeftFeed := false;
end;

procedure TAtomFeed.ParseLine(Line: String; var Item: TFeedItem; var ItemFinished: Boolean);
var
  AFeed: TFeed;
  Elem: TXmlElement;
begin
  inherited ParseLine(Line, Item, ItemFinished);

  if Pos(DCNS, GetCurrentElement.Name) <> 0 then
  begin
    AFeed := TDCFeed.Create;
    Elem := GetCurrentElement;
    StripNS(Elem.Name, DCNS);
    (AFeed as TXmlFeed).Clone(FElemList);
    AFeed.ParseLine(Line, Item, ItemFinished);
{$IFNDEF __GPC__}
    AFeed.Free;
{$ENDIF}
  end;

  FillItem(Item);

  ItemFinished := ((GetCurrentElement.Name = 'entry') and ((GetPreviousElement.Name = 'entry') or (FLeftFeed))) or (Line = SockEof);
  
  if ItemFinished and FLeftFeed then
  begin
    FLeftFeed := false;
  end;
end;

end.
