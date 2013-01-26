unit SaxCallbacks;

{$CALLING cdecl}
{$POINTERMATH ON}

interface

uses
  Expas;

procedure ElementStarted(user_data: Pointer; name: PChar; attrs: PPChar);
procedure ElementEnded(user_data: Pointer; name: PChar);
procedure CharactersReceived(ctx: Pointer; ch: PChar; len: integer);

{$IFDEF EXPAT_2_0}
procedure XmlDeclarationReceived(userData: Pointer; version, encoding: PChar; standalone: integer);
{$ELSE}
{$IFDEF EXPAT_1_0}
function UnknownEncodingDetected(encodingHandlerData:pointer; name:PChar; info:PXML_Encoding):integer;
{$ENDIF}
{$ENDIF}

function DataToUTF8(const InStr, Encoding: PChar): PChar;

implementation

uses
{$IFDEF USE_ICONV}
  Iconv, RProp, RStrings,
{$ENDIF}
  StrTok, SysUtils, Xml;

procedure SplitName(const Name: String; var QName, NameSpace: String);
var
  List: TStringsList;
begin
  List := Split(Name, NameSpaceSeparator);
  if List.Length = 2 then
  begin
    NameSpace := List.Strings[0];
    QName := List.Strings[1];
  end
  else if List.Length = 1 then
  begin
    NameSpace := '';
    QName := List.Strings[0];
  end;
end;

procedure ElementStarted(user_data: Pointer; name: PChar; attrs: PPChar);
var
  attr: String;
  Elem: PXmlElement;
  patt: PXmlAttr;
begin
  with TXmlFeed(user_data) do
  begin
    New(Elem);
    New(Elem^.Attributes, Init);
    WriteStr(attr, name);
    SplitName(attr, Elem^.Name, Elem^.NameSpace);
    Elem^.Content := '';
    Elem^.Depth := Depth;

    if Assigned(attrs) then
    begin
      while Assigned(attrs^) do
      begin
        New(patt);
        WriteStr(attr, attrs^);
        SplitName(attr, patt^.Name, patt^.NameSpace);
        WriteStr(patt^.Value, (attrs + 1)^);

        if patt^.NameSpace = '' then
        begin
          patt^.NameSpace := Elem^.NameSpace;
        end;
        Elem^.Attributes^.Add(patt);

        attr := patt^.Name;
        if attr = 'base' then
        begin
          Elem^.Base := patt^.Value;
        end
        else if attr = 'lang' then
        begin
          Elem^.Lang := patt^.Value;
        end;

        Inc(attrs, 2);
      end;
    end;

    Inc(Depth);
    if Elem^.Name <> '' then
    begin
      FElemList^.Add(Elem);
      SendItem;
    end;
  end;
end;

procedure ElementEnded(user_data: Pointer; name: PChar);
var
  Elem: PXmlElement;
  Ename: STring;
begin
  with TXmlFeed(user_data) do
  begin
    if FElemList^.Count > 0 then
    begin
      WriteStr(Ename, name);
      Elem := FElemList^.GetNth(FElemList^.Count - 1);
      SplitName(Ename, Elem^.Name, Elem^.NameSpace);
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
  WriteStr(enh, ch[0..len - 1]);

  with TXmlFeed(ctx) do
  begin
    if FElemList^.Count > 0 then
    begin
      Elem := FElemList^.GetNth(FElemList^.Count - 1);
      Elem^.Content := Elem^.Content + enh;
      SendItem;
    end;
  end;
end;

{$IFDEF EXPAT_2_0}
procedure XmlDeclarationReceived(userData: Pointer; version, encoding: PChar; standalone: integer);
begin
{$IFDEF USE_ICONV}
  SetProp('charset', StrToPChar('UTF-8'));
{$ENDIF}
  TXmlFeed(userData).FEncoding := encoding;
end;
{$ELSE}
{$IFDEF EXPAT_1_0}
function UnknownEncodingDetected(encodingHandlerData:pointer; name:PChar; info:PXML_Encoding):integer;
begin
  CallMessageEventEx(TXmlFeed(encodingHandlerData), true, UnknownEncoding, StrPas(name));
end;
{$ENDIF}
{$ENDIF}

function DataToUTF8(const InStr, Encoding: PChar): PChar;
var
{$IFDEF USE_ICONV}
  cd: iconv_t;
{$IFDEF USE_LIBICONV}
  i: integer;
{$ENDIF}
  inbytesleft, outbytesleft: size_t;
  inbuf, outbuf: PChar;
{$ENDIF}
  outstr: PChar;
begin
{$IFDEF USE_ICONV}
  if (Encoding <> nil) and (StrIComp('UTF-8', Encoding) <> 0) then
  begin
    cd := iconv_open('UTF-8', Encoding);
    if cd <> iconv_t(-1) then
    begin
  {$IFDEF USE_LIBICONV}
      i := 1;
      iconvctl(cd, ICONV_SET_TRANSLITERATE, @i);
  {$ENDIF}
      if outstr <> nil then
      begin
        inbytesleft := StrLen(InStr);
        outbytesleft := inbytesleft;
        outbuf := outstr;
        if iconv_convert(cd, @inbuf, @inbytesleft, @outbuf, @outbytesleft) = iconv_convert_error then
        begin
          outstr := InStr;
        end;
      end;
    end;
    iconv_close(cd);
  end
  else
  begin
    outstr := InStr;
  end;
{$ELSE}
  outstr := InStr;
{$ENDIF}
  DataToUTF8 := outstr;
end;

end.
