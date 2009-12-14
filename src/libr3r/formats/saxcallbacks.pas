unit SaxCallbacks;

{$CALLING cdecl}

interface

procedure ElementStarted(user_data: Pointer; name: PChar; attrs: PPChar);
procedure ElementEnded(user_data: Pointer; name: PChar);
procedure CharactersReceived(ctx: Pointer; ch: PChar; len: Longint);

implementation

uses
  Xml;

procedure ElementStarted(user_data: Pointer; name: PChar; attrs: PPChar); cdecl;
var
  attr: String;
  i, j: word;
begin
  i := 0;
  j := 0;

  with TXmlFeed(user_data) do
  begin
    FXmlElement.Name := name;
    FXmlElement.Content := '';

    if Assigned(attrs) then
    begin
      while Assigned(attrs^) do
      begin
        attr := String(attrs^);
        StripNS(attr, XMLNSNS);
        attrs^ := PChar(attr);

        if attrs^ = 'base' then
        begin
          FXmlElement.Base := (attrs + 1)^;
        end
        else if attrs^ = 'lang' then
        begin
          FXmlElement.Lang := (attrs + 1)^;
        end
        else if j < 11 then
        begin
          FXmlElement.Attributes[j].Name := attrs^;
          FXmlElement.Attributes[j].Value := (attrs + 1)^;
          Inc(j);
        end;

        Inc(attrs, 2);
        Inc(i, 2);
      end;
    end;

    FElems := Length(FElemList);
    SetLength(FElemList, FElems + 1);
    FElemList[FElems] := FXmlElement;
  end;
end;

procedure ElementEnded(user_data: Pointer; name: PChar); cdecl;
begin
  with TXmlFeed(user_data) do
  begin
    FElems := Length(FElemList);
    if FElems > 0 then
    begin
      SetLength(FElemList, FElems - 1);
    end;

    FXmlElement.Name := LowerCase(name);
  end;
end;

procedure CharactersReceived(ctx: Pointer; ch: PChar; len: Longint); cdecl;
var
  enh: String;
begin
  enh := Copy(ch, 1, len);

  with TXmlFeed(ctx) do
  begin
    FXmlElement.Content := FXmlElement.Content + enh;
  end;
end;

end.
