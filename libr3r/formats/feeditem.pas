unit FeedItem;

interface

type
  TEmail = record
    Toee: String;
    Address: String;
  end;

  TFeedItem = record
    Title: String;
    Links: array of String;
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

    LinksCount: cardinal;
  end;

procedure ClearItem(var Item: TFeedItem);

function CreateEmailRecord(EmailStr: String; const Delim: String; const OffsetEnd: word): TEmail;

implementation

uses
  SysUtils;

procedure ClearItem(var Item: TFeedItem);
begin
  with Item do
  begin
    Title := '';
    Links := nil;
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

function CreateEmailRecord(EmailStr: String; const Delim: String; const OffsetEnd: word): TEmail;
var
  BegName: cardinal;
begin
  EmailStr := Trim(EmailStr);

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
