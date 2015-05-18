unit FeedItem;

interface

type
  TAuthor = record
    Email: String;
    Name: String;
    URI: String;
  end;

  TEnclosure = record
    MimeType: String;
    URL: String;
  end;

  TFeedItem = class
  public
    Title: String;
    Link: String;
    Description: String;
    Subject: String;
    Created: String;
    Contact: TAuthor;
    Generator: String;
    LastModified: String;
    Language: String;
    Id: String;
    Copyright: String;
    Uri: String;
    Myself: String;
    Enclosure: TEnclosure;

    AllowsHTML: Boolean;
    Filtered: Boolean;
    Finished: Boolean;

    constructor Create;
    destructor Destroy; override;
    function DescriptionText: String;
    function TitleText: String;
    procedure Translate;
    procedure Clear;
  end;

function CreateEmailRecord(EmailStr: String; const Delim: String; const OffsetEnd: word): TAuthor;

implementation

uses
  Html4Ent, SysUtils
{$IFDEF USE_ICONV}
  , iconv, RProp, RSettings

{$IFDEF UNIX}
  , cwstring
{$ENDIF}
{$ENDIF};

function DecodeHtml(const InStr: String): String;
var
  i: PtrUInt;
  EntNum: integer;
  EntStr: String;
  ErrPos: byte;
  InEnt, InHTML: Boolean;
  OutStr: String;
begin
  EntStr := '';
  OutStr := '';

  InEnt := false;
  InHTML := false;

  for i := 1 to Length(InStr) do
  begin
    if InStr[i] = '<' then
    begin
      InHTML := true;
    end
    else if (InStr[i] = '&') and
      { if the next character is whitespace, it's not an entity }
      not (InStr[i + 1] in [#0, #9, #10, #13, #32]) then
    begin
      InEnt := true;
    end
    { This has to go here so that > and ; don't get counted
      as part of the text. }
    else if (Not InEnt) and (not InHTML) then
    begin
      OutStr := OutStr + InStr[i];
    end
    else if InStr[i] = '>' then
    begin
      InHTML := false;
      if (Length(OutStr) > 0) and (i < Length(InStr)) and
        { It looks weird to have a space before a puncuation mark }
        not (InStr[i + 1] in ['.', ',', '!', '?', ';', ':']) then
      begin
        OutStr := OutStr + ' ';
      end;
    end
    else if InEnt then
    begin
      if (InStr[i] <> ';') and not (InStr[i] in [#0, #9, #10, #13, #32]) then
      begin
        EntStr := EntStr + InStr[i]
      end
      else
      begin
        EntStr := Html4EntDecode(EntStr);

        if (Length(EntStr) > 0) and (EntStr[1] = '#') then
        begin
          Val(Copy(EntStr, 2, Length(EntStr) - 1), EntNum, ErrPos);
          if (ErrPos = 0) then
          begin
            EntStr := WideChar(EntNum)
          end
          else
          begin
            EntStr := '^' { malformed numerical entity }
          end
        end
        else if Length(EntStr) <> 1 then { undefined entity, per the XML spec }
        begin
          EntStr := '?' { random character for replacing numerical and named references }
        end
        else
        begin
          EntStr := '&' + EntStr { re-prepend an ampersand to non-entities }
        end;

        OutStr := OutStr + EntStr;
        EntStr := '';
        InEnt := false
      end
    end
  end;

  if EntStr <> '' then
  begin
    EntStr := '&' + EntStr;
  end;
  DecodeHtml := OutStr + EntStr;
end;

function StripHtml(const InStr: String): String;
var
  OutStr: String;

procedure ReplChar(const s: String; const c: char);
begin
  while Pos(s, OutStr) <> 0 do
  begin
    OutStr := StringReplace(OutStr, s, c, [rfReplaceAll]);
  end;
end;

begin
  OutStr := DecodeHtml(InStr);
  ReplChar(#9, ' ');
  ReplChar('  ', ' ');
  StripHtml := OutStr;
end;

constructor TFeedItem.Create;
begin
  inherited Create;
  Clear;
end;

destructor TFeedItem.Destroy;
begin
  inherited Destroy;
end;

procedure TFeedItem.Translate;
{$IFDEF USE_ICONV}
var
  cd: iconv_t;
{$IFDEF USE_LIBICONV}
  i: integer;
{$ENDIF}

  incharset, outcharset: PChar;

procedure TranslateField(var FieldValue: String);
var
  inbuf: PChar;
  inbytesleft, outbytesleft: size_t;
  outbuf, outstr: PChar;
begin
  if FieldValue = '' then Exit;
  inbuf := PChar(FieldValue);
  inbytesleft := Length(FieldValue) + 1;
  outbytesleft := inbytesleft;
  GetMem(outstr, outbytesleft);
  if outstr <> nil then
  begin
    outbuf := outstr;
    if iconv_convert(cd, @inbuf, @inbytesleft, @outbuf, @outbytesleft) <> iconv_convert_error then
    begin
      WriteStr(FieldValue, outstr);
    end;
    FreeMem(outstr);
  end;
end;
{$ENDIF}

begin
{$IFDEF USE_ICONV}
  incharset := GetProp('charset');
  outcharset := PChar(String((Settings.GetString('display-encoding'))));

  if incharset = nil then
  begin
    incharset := outcharset;
  end;

  cd := iconv_open(outcharset, incharset);
  if cd <> iconv_t(-1) then
  begin
{$IFDEF USE_LIBICONV}
    i := 1;
    iconvctl(cd, ICONV_SET_TRANSLITERATE, @i);
{$ENDIF}
    TranslateField(Copyright);
    TranslateField(Created);
    TranslateField(Description);
    TranslateField(LastModified);
    TranslateField(Subject);
    TranslateField(Title);

{$IFDEF USE_LIBICONV}
    iconv_close(cd);
  end;
{$ELSE}
  end;

  iconv_close(cd)
{$ENDIF}
{$ENDIF}
end;

procedure TFeedItem.Clear;
begin
  Title := '';
  Description := '';
  Link := '';
  Subject := '';
  Created := '';
  Contact.Email := '';
  Contact.Name := '';
  Contact.URI := '';
  Generator := '';
  LastModified := '';
  Language := '';
  Copyright := '';
  Id := '';
  Uri := '';
  Myself := '';
  Enclosure.MimeType := '';
  Enclosure.URL := '';

  Filtered := false;
  Finished := true;
end;

{ Get the textual representation (without HTML) of the description. }
function TFeedItem.DescriptionText: String;
begin
  if not AllowsHTML then
  begin
    DescriptionText := Description;
  end
  else
  begin
    DescriptionText := StripHtml(Description);
  end;
end;

{ Get the textual representation (without HTML) of the title. }
function TFeedItem.TitleText: String;
begin
  if not AllowsHTML then
  begin
    TitleText := Title;
  end
  else
  begin
    TitleText := StripHtml(Title);
  end;
end;

function CreateEmailRecord(EmailStr: String; const Delim: String; const OffsetEnd: word): TAuthor;
var
  BegName: cardinal;
  Rec: TAuthor;
begin
  BegName := Pos(Delim, EmailStr);
  with Rec do
  begin
    if BegName <> 0 then
    begin
      Email := Copy(EmailStr, 1, BegName - OffsetEnd - 1);
      Name := Copy(EmailStr, BegName + 1, Length(EmailStr) - BegName - OffsetEnd);
    end
    else
    begin
      Email := EmailStr;
    end;
  end;

  CreateEmailRecord := Rec;
end;

end.
