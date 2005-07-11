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

procedure ClearItem(var Item: TFeedItem);

function CreateEmailRecord(EmailStr: String; NameIsDelim: Boolean = true): TEmail;

implementation

uses
  SysUtils;

procedure ClearItem(var Item: TFeedItem);
begin
  with Item do
  begin
    Title := '';
    Links.Clear;
    Description := '';
    Subject := '';
    Created := '';
    Contact.Toee := '';
    Contact.Address := '';
    Generator := '';
    LastModified := '';
    Language := '';
    Copyright := '';
    Id := '';
    Uri := '';
  end;
end;

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
end;

end.
