unit SaxCallbacks;

{$CALLING cdecl}

interface

uses
  Expas;

procedure ElementStarted(user_data: Pointer; name: PChar; attrs: PPXML_Char);
procedure ElementEnded(user_data: Pointer; name: PChar);
procedure {$IFDEF EXPAT_1_0}UnknownDataReceived{$ELSE}CharactersReceived{$ENDIF}(ctx: Pointer; ch: PChar; len: integer);

{$IFDEF EXPAT_1_1}
procedure CdataSectionLimit(userData: Pointer);
{$ENDIF}

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
{$IFNDEF EXPAT_2_0}
  LibR3RStrings, RMessage,
{$ENDIF}
  SysUtils, Xml;

procedure SplitName(const Name: String; var ElemName, NSURI: String);
var
  Index: PtrUInt;
begin
  Index := Pos(NameSpaceSeparator, Name);
  if Index <> 0 then
  begin
    NSURI := Name[1..Index - 1];
    ElemName := Name[Index + 1..Length(Name)];
  end
  else
  begin
    ElemName := Name;
    NSURI := '';
  end;
end;

procedure ElementStarted(user_data: Pointer; name: PChar; attrs: PPXML_Char);
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
  Ename: String;
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

procedure {$IFDEF EXPAT_1_0}UnknownDataReceived{$ELSE}CharactersReceived{$ENDIF}(ctx: Pointer; ch: PChar; len: integer);
var
  Elem: PXmlElement;
  enh: String;
  loff: 0..1;
{$IFDEF EXPAT_1_0}
  clen, cpos: integer;
  IsEntity: Boolean;
{$ENDIF}
begin
  loff := Ord(ch[len-1] = #9);
  WriteStr(enh, ch[0..len - (loff + 1)]);

  with TXmlFeed(ctx) do
  begin
    if FElemList^.Count > 0 then
    begin
      Elem := FElemList^.GetNth(FElemList^.Count - 1);

{$IFDEF EXPAT_1_0}
      cpos := Pos('&', Elem^.Content);
      clen := Length(Elem^.Content);
      IsEntity := cpos <> 0;
      if (clen > 0) and (Elem^.Content[clen] <> ' ') and not IsEntity then
      begin
        Elem^.Content := Elem^.Content + ' ';
      end;
{$ENDIF}
      Elem^.Content := Elem^.Content + enh;
      SendItem;
    end;
  end;
end;

{$IFDEF EXPAT_1_1}
procedure CdataSectionLimit(userData: Pointer);
begin
end;
{$ENDIF}

{$IFDEF EXPAT_2_0}
procedure XmlDeclarationReceived(userData: Pointer; version, encoding: PChar; standalone: integer);
begin
{$IFDEF USE_ICONV}
  SetProp('charset', StrToPCharAlloc('UTF-8'));
{$ENDIF}
  TXmlFeed(userData).FEncoding := encoding;
end;
{$ELSE}
{$IFDEF EXPAT_1_0}
function UnknownEncodingDetected(encodingHandlerData:pointer; name:PChar; info:PXML_Encoding):integer;
var
  Message: String;
begin
  WriteStr(Message, name);
  CallMessageEvent(TXmlFeed(encodingHandlerData), true, UnknownEncoding, Message);
  UnknownEncodingDetected := 0;
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
        outbytesleft := 0;
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
