unit FeedItem;

{$H+}

interface

uses
  StrTok;

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

    Finished: Boolean;

    constructor Create;
    destructor Destroy; {$IFNDEF __GPC__}override;{$ENDIF}
    function DescriptionText: String;
    function TitleText: String;
    procedure Translate;
    procedure Clear;
  end;

function CreateEmailRecord(EmailStr: String; const Delim: String; const OffsetEnd: word): TAuthor;

implementation

uses
{$IFDEF USE_ICONV}
  iconv, RProp, RSettings,
{$ENDIF}

{$IFDEF UNIX}
  cwstring
{$ENDIF};

function StripHtml(const InStr: String): String;
var
  EntNum: integer;
  EntStr: String;
  ErrPos: byte;
  i: cardinal;
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
    else if not InHTML and (InStr[i] = '&') and
      { if the next character is whitespace, it's not an entity }
      not (InStr[i + 1] in [#0, #8, #9, #10, #13, #32]) then
    begin
      InEnt := true;
    end
    { This has to go here so that > and ; don't get counted
      as part of the text. }
    else if (not InEnt) and (not InHTML) then
    begin
      OutStr := OutStr + InStr[i];
    end
    else if InStr[i] = '>' then
    begin
      InHTML := false;
    end
    else if InEnt  then
    begin
      if InStr[i] <> ';' then
      begin
        EntStr := EntStr + InStr[i];
      end
      else
      begin
        if EntStr = 'amp' then
        begin
          EntStr := '&';
        end
        else if EntStr = 'gt' then
        begin
          EntStr := '>';
        end
        else if EntStr = 'lt' then
        begin
          EntStr := '<';
        end
        else if EntStr = 'quot' then
        begin
          EntStr := '"';
        end
        else if Copy(EntStr, 1, 1) = '#' then { numerical entity }
        begin
          Val(Copy(EntStr, 2, Length(EntStr) - 1), EntNum, ErrPos);
          if (ErrPos = 0)
          {$IFDEF __GPC__}
            and (EntNum < 256)
          {$ENDIF}
            then
          begin
            EntStr := WideChar(EntNum)
          end
          else
          begin
            EntStr := #164 { random character for malformed numerical entity }
          end
        end
        else { undefined entity, per the XML spec }
        begin
          EntStr := #164; { random character for unknown named references }
        end;

        OutStr :=  OutStr + EntStr; { random character for replacing numerical and named references }
        EntStr := '';
        InEnt := false;
      end
    end;
  end;

  StripHtml := OutStr;
end;

constructor TFeedItem.Create;
begin
{$IFNDEF __GPC__}
  inherited Create;
{$ENDIF}

  Finished := false;
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
  outbytesleft := 0;
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

  Finished := false;
end;

{ Get the textual representation (without HTML) of the description. }
function TFeedItem.DescriptionText: String;
begin
  DescriptionText := StripHtml(Description);
end;

{ Get the textual representation (without HTML) of the title. }
function TFeedItem.TitleText: String;
begin
  TitleText := StripHtml(Title);
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
