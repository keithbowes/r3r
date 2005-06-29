unit FeedItem;

interface

uses
  Classes;

type
  TEmail = record
    Toee: String;
    Address: String;
  end;

  TFeedItem = record
    Title: String;
    Links: TStringList;
    Description: String;
    Subject: String;
    Created: String;
    Contact: TEmail;
    Generator: String;
    LastModified: String;
    Language: String;
    Copyright: String;
    Id: String;
    Uri: String;
  end;

function CreateEmailRecord(EmailStr: String; NameIsDelim: Boolean = true): TEmail;

implementation

uses
  SysUtils;

function CreateEmailRecord(EmailStr: String; NameIsDelim: Boolean = true): TEmail;
var
  BegName: cardinal;
  Delim: String;
  OffsetEnd: 0..1;
begin
  EmailStr := Trim(EmailStr);

  if NameIsDelim then
  begin
    Delim := '(';
    OffsetEnd := 1;
  end
  else
  begin
    Delim := ' ';
    OffsetEnd := 0;
  end;

  BegName := Pos(Delim, EmailStr);
  WriteLn(BegName);
  with Result do
  begin
    if BegName <> 0 then
    begin
      Address := Copy(EmailStr, 1, BegName - OffsetEnd - 1);
      Toee := Copy(EmailStr, BegName + 1, Length(EmailStr) - BegName - OffsetEnd);
    end
    else
    begin
      Address := EmailStr;
    end;
  end;
  WriteLn('Address: ', Result.Address);
  WriteLn('Toee: ', Result.Toee);
end;

end.
