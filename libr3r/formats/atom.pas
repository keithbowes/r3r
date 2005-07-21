unit Atom;

interface

uses
  Feed, FeedItem, Xml;

type
  TAtomFeed = class(TXmlFeed)
  private
    { Category Type }
    FCatType: String;
  protected
    function GetFormat: TFeedType; override;
    procedure FillItem(var Item: TFeedItem);
  public
    constructor Create;
    procedure ParseLine(Line: String; var Item: TFeedItem; var ItemFinished: Boolean); override;
  end;

implementation

uses
  SockConsts, SysUtils;

function TAtomFeed.GetFormat: TFeedType;
begin
  Result := ftAtom;
end;

procedure TAtomFeed.FillItem(var Item: TFeedItem);
var
  Idx: integer;
begin
  with Item, FXmlElement do
  begin
    Language := Lang;

    if Name = 'title' then
    begin
      Title := Title + Content;
    end
    else if Name = 'summary' then
    begin
      Description := Description + Content;
    end
    else if Name = 'link' then
    begin
      LinksCount := Length(Links);
      SetLength(Links, LinksCount + 1);

      for Idx := Low(Attributes) to High(Attributes) do
      begin
        if Attributes[Idx].Name = 'href' then
        begin
          Links[LinksCount] := Attributes[Idx].Value;
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
    else if Name = 'name' then
    begin
      if PreviousElement.Name = 'email' then
      begin
        Contact.Toee := Contact.Toee + Content;
      end;
    end
    else if Name = 'uri' then
    begin
      if PreviousElement.Name = 'email' then
      begin
        Contact.Address := Contact.Address + Content;
      end;
    end
    else if Name = 'generator' then
    begin
      Generator := Generator + Content;
    end
    else if Name = 'updated' then
    begin
      LastModified := LastModified + Content;
    end
    else if Name = 'rights' then
    begin
      Copyright := Copyright + Content;
    end
    else if Name = 'id' then
    begin
      Id := Id + Content;
      Uri := Id;
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
