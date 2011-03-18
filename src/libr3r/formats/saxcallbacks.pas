unit SaxCallbacks;

{$CALLING cdecl}

interface

uses
  Expas;

procedure ElementStarted(user_data: Pointer; name: PChar; attrs: PPChar);
procedure ElementEnded(user_data: Pointer; name: PChar);
procedure CharactersReceived(ctx: Pointer; ch: PChar; len: integer);
procedure InstructionReceived(userData: Pointer; target, data: PChar);

implementation

uses
{$IFDEF USE_ICONV}
  RProp, RStrings,
{$ENDIF}
  SysUtils, Xml;

procedure ElementStarted(user_data: Pointer; name: PChar; attrs: PPChar);
var
  attr: String;
  Elem: PXmlElement;
  i: word;
begin
  i := 0;

  with TXmlFeed(user_data) do
  begin
    New(Elem);
    Elem^.Name := StrPas(name);
    Elem^.Content := '';
    Elem^.Depth := Depth;

    if Assigned(attrs) then
    begin
      while Assigned(attrs^) do
      begin
        attr := StrPas(attrs^);
        StripNS(attr, XMLNSNS);
        attrs^ := PChar(attr);

        if attr = 'base' then
        begin
          Elem^.Base := StrPas((attrs + 1)^);
        end
        else if attr = 'lang' then
        begin
          Elem^.Lang := StrPas((attrs + 1)^);
        end
        else if i < 11 then
        begin
          Elem^.Attributes[i].Name := attr;
          Elem^.Attributes[i].Value := StrPas((attrs + 1)^);
          Inc(i);
        end;

        Inc(attrs, 2);
      end;
    end;

    Inc(Depth);
    if Elem^.Name <> '' then
    begin
      FElemList^.Add(Elem);
      SendItem(Elem^.Name, '');
    end;
  end;
end;

procedure ElementEnded(user_data: Pointer; name: PChar);
var
  ELem: PXmlElement;
begin
  with TXmlFeed(user_data) do
  begin
    if FElemList^.Count > 0 then
    begin
      Elem := FElemList^.GetNth(FElemList^.Count - 1);
      Elem^.Name := LowerCase(StrPas(name));
    end;

    if Elem^.Name <> '' then
    begin
      Dec(Depth);
    end;
  end;
end;

procedure CharactersReceived(ctx: Pointer; ch: PChar; len: integer);
var
  Elem: PXmlElement;
  enh: String;
begin
  enh := Copy(StrPas(ch), 1, len);

  with TXmlFeed(ctx) do
  begin
    if FElemList^.Count > 0 then
    begin
      Elem := FElemList^.GetNth(FElemList^.Count - 1);
      Elem^.Content := Elem^.Content + enh;
      SendItem(Elem^.Name, Elem^.Content);
    end;
  end;
end;

procedure InstructionReceived(userData: Pointer; target, data: PChar);
{$IFDEF USE_ICONV}
var
  Priv: String;
  PrivStart: word;
{$ENDIF}
begin
{$IFDEF USE_ICONV}
  if StrComp(target, 'xml') <> 0 then
  begin
    if GetProp('charset') = nil then
    begin
      Priv := StrPas(data);
      PrivStart := Pos('encoding=', Priv);
      if PrivStart <> 0 then
      begin
        Delete(Priv, 1, PrivStart + Length('encoding='));
        PrivStart := Pos('''', Priv);
        Delete(Priv, PrivStart, Length(Priv) - PrivStart + Length(''''));
        PrivStart := Pos('"', Priv);
        Delete(Priv, PrivStart, Length(Priv) - PrivStart + Length('"'));
        SetProp('charset', StrToPChar(Priv));
      end;
    end;
  end;
{$ENDIF}
end;

end.
