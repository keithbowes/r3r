unit FeedItem;

{$H+}

interface

uses
  RList, StrTok;

type
  PEmail = ^TEmail;
  TEmail = record
    Toee: String;
    Address: String;
  end;

  TEnclosure = record
    MimeType: String;
    URL: String;
  end;

  TFeedItem = class
  protected
    procedure Cleanup;
  public
    Title: String;
    Links: PRList;
    MainLink: String;
    Description: String;
    Subject: String;
    Created: String;
    Contact: PEmail;
    Generator: String;
    LastModified: String;
    Language: String;
    Id: String;
    Copyright: String;
    Uri: String;
    Myself: String;
    Enclosure: TEnclosure;

    constructor Create;
    destructor Destroy; {$IFNDEF __GPC__}override;{$ENDIF}
    function LinksCount: cardinal;
    function GetMainLink: String;
    function GetPodcast: String;
    procedure Clear;
  end;

function CreateEmailRecord(EmailStr: String; const Delim: String; const OffsetEnd: word): TEmail;

implementation

uses
  SysUtils;

var
  AlreadyAllocated: Boolean = false;

constructor TFeedItem.Create;
begin
{$IFNDEF __GPC__}
  inherited Create;
{$ENDIF}

  New(Contact);

  if not AlreadyAllocated then
  begin
    New(Links, Init);
    AlreadyAllocated := true;
  end;
end;

destructor TFeedItem.Destroy;
begin
  Dispose(Contact);
  Cleanup;

{$IFNDEF __GPC__}
  inherited Destroy;
{$ENDIF}
end;

procedure TFeedItem.Cleanup;
begin
  if AlreadyAllocated then
  begin
    if Links <> nil then
    begin
      Links^.Add(nil); // Hack: Have at least one node to destroy
      Dispose(Links, Done);
      AlreadyAllocated := false;
    end;
  end;
end;

procedure TFeedItem.Clear;
begin
  Title := '';
  Description := '';
  MainLink := '';
  Subject := '';
  Created := '';
  Contact^.Toee := '';
  Contact^.Address := '';
  Generator := '';
  LastModified := '';
  Language := '';
  Copyright := '';
  Id := '';
  Uri := '';
  Myself := '';

  Links^.Add(nil);
end;

function TFeedItem.LinksCount: cardinal;
begin
  LinksCount := Links^.Count;
end;

function TFeedItem.GetMainLink: String;
var
  CurrentLink: cardinal;
  Link: String;
begin
  CurrentLink := LinksCount;
  if (Links <> nil) and (LinksCount > 0) then
  begin
    repeat
      Link := StrPas(Links^.GetNth(CurrentLink));
      Dec(CurrentLink);
    until (Link <> '') or (CurrentLink = LinksCount);
  end
  else
  begin
    Link := '';
  end;

  if Link <> '' then
  begin
    MainLink := Link;
  end;

  GetMainLink := MainLink;
end;

function TFeedItem.GetPodcast: String;
begin
  if (Pos('audio', Enclosure.MimeType) = 1) or
    (Pos('video', Enclosure.MimeType) = 1) then
  begin
    GetPodcast := Enclosure.URL;
  end
  else
  begin
    GetPodcast := '';
  end;
end;

function CreateEmailRecord(EmailStr: String; const Delim: String; const OffsetEnd: word): TEmail;
var
  BegName: cardinal;
  Rec: TEmail;
begin
  EmailStr := Trim(EmailStr);

  BegName := Pos(Delim, EmailStr);
  with Rec do
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

  CreateEmailRecord := Rec;
end;

end.
