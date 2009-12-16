unit SaxCallbacks;

{$CALLING cdecl}

interface

uses
  Expas;

procedure ElementStarted(user_data: Pointer; name: PChar; attrs: PPChar);
procedure ElementEnded(user_data: Pointer; name: PChar);
procedure CharactersReceived(ctx: Pointer; ch: PChar; len: integer);

implementation

uses
  RStrings, Xml
{$IFNDEF FPC}
  , SysUtils
{$ENDIF};

procedure ElementStarted(user_data: Pointer; name: PChar; attrs: PPChar);
var
  attr: String;
  Elem: PXmlElement;
  i, j: word;
begin
  i := 0;
  j := 0;

  with TXmlFeed(user_data) do
  begin
    New(Elem);
    Elem^.Name := StrPas(name);
    Elem^.Content := '';

    if Assigned(attrs) then
    begin
      while Assigned(attrs^) do
      begin
        attr := String(attrs^);
        StripNS(attr, XMLNSNS);
        attrs^ := PChar(attr);

        if StrPas(attrs^) = 'base' then
        begin
          Elem^.Base := StrPas((attrs + 1)^);
        end
        else if StrPas(attrs^) = 'lang' then
        begin
          Elem^.Lang := StrPas((attrs + 1)^);
        end
        else if j < 11 then
        begin
          Elem^.Attributes[j].Name := StrPas(attrs^);
          Elem^.Attributes[j].Value := StrPas((attrs + 1)^);
          Inc(j);
        end;

        Inc(attrs, 2);
        Inc(i, 2);
      end;
    end;

    FElemList^.Add(ELem);
  end;
end;

procedure ElementEnded(user_data: Pointer; name: PChar);
var
  ELem: PXmlElement;
  i: word;
begin
  with TXmlFeed(user_data) do
  begin
    if FElemList^.Count > 0 then
    begin
      Elem := FElemList^.GetNth(FElemLIst^.Count - 1);
      Elem^.Name := LowerCase(StrPas(name));
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
    Elem := FElemList^.GetNth(FElemList^.Count - 1);
    Elem^.Content := enh;
  end;
end;

end.
