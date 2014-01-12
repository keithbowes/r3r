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
    Finished: Boolean;

    constructor Create;
    destructor Destroy; {$IFNDEF __GPC__}override;{$ENDIF}
    function DescriptionText: String;
    function TitleText: String;
    procedure Translate;
    procedure Clear;
  end;

function CreateEmailRecord(EmailStr: String; const Delim: String; const OffsetEnd: word): TAuthor;
function DecodeHtml(const InStr: String): String;

implementation

uses
  SysUtils
{$IFDEF USE_ICONV}
  , iconv, RProp, RSettings

{$IFDEF UNIX}
  , cwstring
{$ENDIF}
{$ENDIF};

function DecodeHtml(const InStr: String): String;
var
  ErrPos: byte;
  i: PtrUInt;
  InHTML: Boolean;
  OutStr: String;
begin
  OutStr := '';

  InHTML := false;

  for i := 1 to Length(InStr) do
  begin
    if InStr[i] = '<' then
    begin
      InHTML := true;
    end
    { This has to go here so that > doesn't get counted
      as part of the text. }
    else if not InHTML then
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
  end;
  DecodeHtml := OutStr;
end;

function StripHtml(const InStr: String): String;
var
  OutStr: String;
begin
  OutStr := DecodeHtml(InStr);
  while Pos('  ', OutStr) <> 0 do
  begin
    OutStr := StringReplace(OutStr, '  ', ' ', [rfReplaceAll]);
  end;
  StripHtml := OutStr;
end;

constructor TFeedItem.Create;
begin
{$IFNDEF __GPC__}
  inherited Create;
{$ENDIF}

  Clear;
end;

destructor TFeedItem.Destroy;
begin
{$IFNDEF __GPC__}
  inherited Destroy;
{$ENDIF}
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
{$IFNDEF __GPC__}
  inbuf := PChar(FieldValue);
{$ELSE}
  inbuf := FieldValue;
{$ENDIF}
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
{$IFNDEF __GPC__}
  outcharset := PChar(String((Settings.GetString('display-encoding'))));
{$ELSE}
  outcharset := Settings.GetString('display-encoding');
{$ENDIF}

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
